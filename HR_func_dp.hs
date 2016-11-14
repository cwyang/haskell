-- Password Cracker : NOT PASS: XXX
{-
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Ord
import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Debug.Trace

solve :: [B.ByteString] -> B.ByteString -> [[B.ByteString]]
solve _ "" = [[]]
solve passlist target
  | trace ("myfun " ++ show passlist ++ " " ++ show target) False = undefined
  | null matched = []
  | otherwise = concat [ map (x:) $ solve passlist (B.drop (B.length x) target) | x <- matched ]
  where matched = filter (`B.isPrefixOf` target) passlist

readCase :: IO (B.ByteString, [B.ByteString], B.ByteString)
readCase = (,,) <$> B.getLine <*> (B.words <$> B.getLine) <*> B.getLine
  
main = do
  t <- (fst . fromJust . B.readInt) <$> B.getLine
  replicateM_ t $ do
    (_,pass,target) <- readCase
    let res = case solve (reverse . sortBy (comparing B.length) $ pass) target of
          [] -> "WRONG PASSWORD"
          x:xs -> B.unwords x
    B.putStrLn res
7
-- HMM..
foo = "a aa aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa aaaaaaaaa aaaaaaaaaa"
foo' = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
-- Solution from HR

{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.ByteString.Char8 as BS -- BS.getContents
import qualified Data.Vector as V

main :: IO ()
main = BS.getContents >>= mapM_ putStrLn. validate. BS.lines

validate :: [BS.ByteString] -> [String]
validate (t':left) = parse left
  where
    t = readInt t'

parse :: [BS.ByteString] -> [String]
parse [] = []
parse (n':pass':login:rest) = solve (V.fromList pass) login : parse rest
  where
    n = readInt n'
    pass = BS.words pass'

solve :: V.Vector BS.ByteString -> BS.ByteString -> String
solve pass login = if V.last par == (-1)
                    then "WRONG PASSWORD"
                    else BS.unpack. BS.unwords. reverse. getPath $ (len-1)
  where
    len = BS.length login
    nPass = V.length pass
    par :: V.Vector Int
    par = V.fromList [solveIdx idx | idx <- [0.. (len-1)]]
    getPath :: Int -> [BS.ByteString]
    getPath (-1) = []
    getPath idx = pass V.! passIdx: getPath (idx - BS.length (pass V.! passIdx))
      where
        passIdx = par V.! idx
    solveIdx :: Int -> Int
    solveIdx idx
      | idx < 0 = -1
      | otherwise = (\x -> if null x then (-1) else head x)
                      [   i | i <- [0.. (nPass-1)]
                        , matches idx (pass V.! i) login
                        , let lstIdx = idx - BS.length (pass V.! i)
                          in ((lstIdx == -1) || (par V.! lstIdx /= -1))]

matches :: Int -> BS.ByteString -> BS.ByteString ->  Bool
matches endPos source dest
  | stIdx < 0 = False
  | otherwise = source `BS.isPrefixOf` dest1
  where
    len = BS.length source
    stIdx = endPos - len + 1
    dest1 = BS.drop stIdx dest

readInt = (\(Just (x_yzpqr,_)) -> x_yzpqr). BS.readInt
foo :: [BS.ByteString]
foo' :: BS.ByteString
foo = BS.words "a aa aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa aaaaaaaaa aaaaaaaaaa"
foo' = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
-}
-- Different Ways
{-
choose :: Int -> Int -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * fromIntegral n `div` fromIntegral k
calc :: [Int] -> Integer
calc [n,k] = choose n k `rem` (10^8+7)
main = do
  [n] <- rl
  mapM_ (\_ -> do
            l <- rl
            putStrLn . show . calc $ l)
    [1..n]
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- Dice Path
import Control.Monad
import Debug.Trace
import Data.List.Split
data Dice = Dice { top :: Int
                 , front :: Int
                 , right :: Int } deriving Show

rightRoll, downRoll :: Dice -> Dice
rightRoll d = Dice (7-right d) (front d) (top d)
downRoll d  = Dice (7-front d) (top d) (right d)

--solve :: Int -> Int -> Int
solve m n = go m n -- fst $ go m n
  where go :: Int -> Int -> (Int, [Dice])
        go 1 1 = (1, [Dice 1 2 4])
        go 1 n = let (lv, [ld]) = go 1 (n-1)
                     d        = [rightRoll ld]
                 in (lv + top d, d)
        go m 1 = let (uv, [ud]) = go (m - 1) 1
                     d        = [downRoll ud]
                 in (uv + top d, d)
        go m n = let (uv, uds) = go (m - 1) n
                     (lv, lds) = go m (n-1)
                     lds'      = map rightRoll lds
                     uds'      = map downRoll uds
                 in case compare uv lv of
                      LT -> (lv+top d, [ld'])
                      RT -> (uv+top d, [ud'])
                      EQ -> (lv+top d, [ld', ud']
foo m n = putStrLn . unlines . map (unwords . map show) $ chunksOf n [solve y x | y <- [1..m], x <- [1..n]]

main = do
  [t] <- rl
  qs <- replicateM t $ rl >>= (return .tuple)
  mapM_ (putStrLn . show . uncurry solve) qs
  where rl = fmap (map (read :: String->Int) . words) getLine
        tuple [a,b] = (a,b)
            
