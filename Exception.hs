{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import System.Random (randomRIO)
import System.IO
import Control.Exception
import Data.Typeable
import Control.Monad.Trans.Except
data MyException =
  forall e.
  (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) =
    showsPrec p e

multiError :: Int -> Either MyException Int

multiError n | n == 0 = Left (MyException DivideByZero)
             | n == 1 = Left (MyException StackOverflow)
             | otherwise = Right n

data SomeError = Arith ArithException
               | Async AsyncException
               | SomethingElse
               deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing ->
      case cast e of
        (Just async) -> Async async
        Nothing -> SomethingElse

runDisc n =
  either discriminateError
  (const SomethingElse) (multiError n)

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn $ "We errored! It was: " ++ show e

foo = writeFile "/jj" "hi"
  `catch` handler

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e

randomException :: IO ()
randomException = do
  i <- randomRIO(1,10 ::Int)
  if i `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

main :: IO()
main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "looping"
  threadDelay (1 * 1000000)


data NotDivThree = NotDivThree Int deriving (Eq, Show)
instance Exception NotDivThree
data NotEven = NotEven Int deriving (Eq, Show)
instance Exception NotEven -- derivable

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i = throwIO $ NotEven i
  | otherwise = return i

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch
catchNotEven :: IO Int -> (NotEven -> IO Int) -> IO Int
catchNotEven = catch

catchFoo :: IO Int -> IO Int
catchFoo ioInt =
  catches ioInt
  [ Handler (\(NotEven _) -> return maxBound)
  , Handler (\(NotDivThree _) -> return minBound)
    ]

data EATD = NotEven' Int
          | NotDivThree' Int
          deriving (Eq, Show)
instance Exception EATD

bottomTest :: IO (Either SomeException ())
bottomTest = try undefined
bottomTest' :: IO (Either SomeException ())
bottomTest' = try $ return undefined

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "/tmp/test.data" WriteMode
  threadDelay 15
  hPutStr h ("hello"++replicate 9999999 '0' ++ "abc")
  hClose h

data PleaseDie = PleaseDie deriving Show
instance Exception PleaseDie
die :: IO ()
die = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 100000
  throwTo threadId PleaseDie
