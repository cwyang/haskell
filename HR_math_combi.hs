-- A Chocolate Fiesta
-- Sum(even k) [nCk] = 2^(n-1)
-- Sum(odd  k) [nCk] = 2^(n-1)
{- 1st try
import qualified Data.ByteString.Char8 as B
import Data.Maybe
choose :: Int -> Int -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * fromIntegral n `div` fromIntegral k
count n l = (2^numEven) * (oddWay+1) - 1
  where numOdd = length . filter odd $ l
        numEven = n - numOdd
        oddWay = sum $ map (\x -> choose numOdd x) [2,4..numOdd]
main = do
  [n] <- rl
  l <- rl
  print $ count n l `rem` (10^9+7)
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLinep :: Int
-}
{- 2nd try with modular arithmetic
p = 10^9+7
chooseModP :: Int -> Int -> Int -> Int
chooseModP n 0 _ = 1
chooseModP 0 k _ = 0
chooseModP n k p = (v * chooseModP (n-1) (k-1) p) `rem` p
  where v = ((n `rem` p) * inv k p) `rem` p
expModP :: Int -> Int -> Int -> Int -- a^k mod p
expModP _ 0 _ = 1
expModP a k p
  | odd k     = (a * expModP a (k-1) p) `rem` p
  | otherwise = expModP (a^2 `rem` p) (k `div` 2) p 
-- Fermat's Theorem
-- a^(p-1) = 1
-- a*a^(p-2) = 1
-- inv a = a^(p-2)
inv :: Int -> Int -> Int
inv k p = expModP k (p-2) p
-}
-- 3rd try with web hint
{-
count n l = (2^numEven) * oddWay - 1
  where numOdd = length . filter odd $ l
        numEven = n - numOdd
        oddWay = if numOdd == 0 then 1 else 2^(numOdd-1)
main = do
  [n] <- rl
  l <- rl
  print $ count n l `rem` (10^9+7)
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- Picking Cards
import Data.Array
calc :: (Int,[Int]) -> Integer
calc (n,l) = let arr = accumArray (+) 0 (0,n) [ (x,1) | x <- l, x < n ]
             in go arr 0 0
  where go arr idx choice
          | idx == n = 1
          | choiceCnt == 0 = 0
          | otherwise      = choiceCnt * (go arr (idx+1) (choiceCnt-1))
          where choiceCnt = arr ! idx + choice

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . calc) . doArg . lines
  where doArg [] = []
        doArg (n:x:xs) = (read n,rl x):doArg xs
        rl x = map read . words $ x
