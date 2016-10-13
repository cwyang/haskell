import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Either
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Typeable
import Network.HTTP
import System.IO
import Text.Printf
import System.CPUTime

-- fork
main1 = do
  hSetBuffering stdout NoBuffering
  forkIO $ replicateM_ 100000 (putChar 'A')
  replicateM_ 100000 (putChar 'B')

-- reminder
main2 = loop
  where
    loop = do
      s <- getLine
      if s == "q"
        then return ()
        else do forkIO $ setReminder s
                loop
setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, see ya in %d secs" t
  threadDelay (10^6 * t)
  printf "%d secs is up! BING! \BEL\n" t

-- MVar
{-
data MVar a
newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
-}
main3 = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 1; putMVar m 3;putMVar m 2; putMVar m 4
  threadDelay (10^6)
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

foo = do
  m <- newEmptyMVar
  takeMVar m

-- Logger

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      threadDelay (1000)
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "Logger Stop"
          putMVar s ()
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m $ Message s
logStop :: Logger -> IO ()
logStop (Logger m) = do
  n <- newEmptyMVar
  putMVar m $ Stop n
  takeMVar n

main4 = do
  l <- initLogger
  logMessage l "Foo"
  logMessage l "Bar"
  logStop l

-- MVar for Lock:  lock == take MVar

type Name = String
type PhoneNumber = String
type PhoneBook = M.Map Name PhoneNumber
newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar M.empty
  return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name num = do
  book <- takeMVar m
  putMVar m $! M.insert name num book -- strict apply
insert' (PhoneBookState m) name num = do
  book <- takeMVar m
  let book' = M.insert name num book
  putMVar m book'
  seq book' $ return () -- strict and short trick

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ M.lookup name book

main5 = do
  s <- new
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  Main.lookup s "name999" >>= print
  Main.lookup s "unknown" >>= print

{- Control.Concurrent.Chan
data Chan a

newChan :: IO (Chan a)
readChan :: Chan a -> IO a
writeChan :: Chan a -> a -> IO ()
-}

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)
data Chan' a
     = Chan' (MVar (Stream a)) (MVar (Stream a))
newChan' :: IO (Chan' a)
newChan' = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return $ Chan' readVar writeVar

writeChan' :: Chan' a -> a -> IO ()
writeChan' (Chan' _ w) a = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar w
  putMVar oldHole $ Item a newHole
  putMVar w newHole

readChan' :: Chan' a -> IO a
readChan' (Chan' r _) = do
  stream <- takeMVar r
  Item v next <- readMVar' stream
  putMVar r next
  return v

dupChan' :: Chan' a -> IO (Chan' a)
dupChan' (Chan' _ w) = do
  hole <- readMVar' w
  r <- newMVar hole
  return $ Chan' r w

readMVar' :: MVar a -> IO a
readMVar' m = do
  a <- takeMVar m
  putMVar m a
  return a

unGetChan :: Chan' a -> a -> IO () -- deadlock with read pending
unGetChan (Chan' r _) val = do
  newReadEnd <- newEmptyMVar
  readEnd <- takeMVar r
  putMVar newReadEnd $ Item val readEnd
  putMVar r newReadEnd
  
chanTest = do
  c <- newChan'
  d <- dupChan' c
  writeChan' c "hello"
  writeChan' c "world"
  writeChan' c "quux"
  replicateM_ 3 $ readChan' c >>= print
  replicateM_ 3 $ readChan' d >>= print
  
-- Async Get
-- no https
httpGet :: String -> IO String
httpGet url = simpleHTTP (getRequest url) >>= getResponseBody
httpGetCode :: String -> IO ResponseCode
httpGetCode url = simpleHTTP (getRequest url) >>= getResponseCode

httpTest = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  forkIO $ do
    r <- httpGet "http://www.google.com/"
    putMVar m1 r
  forkIO $ do
    r <- httpGet "http://www.yahoo.com/"
    putMVar m2 r

  r1 <- takeMVar m1
  r2 <- takeMVar m2
  print (take 100 r1, take 100 r2)

data Async a = Async (MVar a)
async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO $ do
    r <- action
    putMVar var r
  return (Async var)
wait :: Async a -> IO a
wait (Async va) = readMVar va

timehttpTest2 = do
  a1 <- async (httpGet "http://www.google.com/")
  a2 <- async (httpGet "http://www.yahoo.com/")
  r1 <- wait a1
  r2 <- wait a2
  putStrLn $ printf "%s\n%s" r1 r2

sites = [ "http://www.google.com/"
        , "http://www.yahoo.com/"
        ]
timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeIt $ httpGet url
  printf "downloaded: %s (%d bytes, %.5fs)\n"
    url (length page) time
    
timeIt :: IO a -> IO (a, Double)
timeIt ioa = do
  t1 <- getCPUTime
  a <- ioa
  t2 <- getCPUTime
  let t :: Double
      t = fromIntegral (t2 - t1) * 1e-12
  return (a, t)
  
timehttpTest3 = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as

-- Exception
data MyException = MyException deriving (Show, Typeable)
instance Exception MyException

-- Async Error Handling with threadid

data Async' a = Async' ThreadId (MVar (Either SomeException a))

cancel' :: Async' a -> IO()
cancel' (Async' t var) = throwTo t ThreadKilled

async' :: IO a -> IO (Async' a)
async' action = do
  var <- newEmptyMVar
  t <- forkIO $ do
    r <- try action  -- try is added
    putMVar var r
  return $ Async' t var

waitCatch :: Async' a -> IO (Either SomeException a)
waitCatch (Async' _ var) = readMVar var

wait' :: Async' a -> IO a
wait' a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a -> return a

main6 = do
  as <- mapM (async' . timeDownload) sites

  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ mapM_ cancel' as
  rs <- mapM waitCatch as
  printf "%d/%d succeded\n" (length $ rights rs) (length rs)

sites2 = [ "http://www.google.com/"
         , "http://www.yahoo.com/"
         , "http://www.naver.com/"
        ]

timehttpTest5 = do
  m <- newEmptyMVar
  let
    download url = do
      r <- httpGet url
      return (url, r)
  as <- mapM (async . download) sites2

  (url, r) <- waitAny as
  printf "%s was first (%d bytes)\n" url $ length r
  mapM_ wait as

waitAny :: [Async a] -> IO a
waitAny as = do
  m <- newEmptyMVar
  let forkwait a =
        forkIO $ do
        r <- wait a
        putMVar m r
  mapM_ forkwait as
  wait (Async m)
      

