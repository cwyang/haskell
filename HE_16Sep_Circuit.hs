-- Print HackerEarth
-- hackerearth == aa c ee hh k rr t
{-
import Data.List
import Data.Char
calc :: [Char] -> Int
calc s = minimum [f 'a' `div` 2,
                  f 'e' `div` 2,
                  f 'h' `div` 2,
                  f 'r' `div` 2,
                  f 'c', f 'k', f 't'] - 1
  where l' = map length . group . sort $ s ++ ['a'..'z'] ++ ['a'..'z'] 
        f c = l' !! (ord c - ord 'a')
main = getLine >> getLine >>=
  print . calc
-}
-- Mishki Playing Games
{-
module Main where
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Data.List
import Data.Maybe
import Data.Array

myLog :: Int -> Int
myLog 0 = 0
myLog n = 1 + myLog (n `div` 2)

calc :: Int -> Int -> Array Int Int -> Bool
calc l r arr = odd $ arr ! r - arr ! (l-1) 
foo n = listArray (0,n) . scanl' (+) 0 . map myLog $ [1..]
main = do
  [n,q] <- rl
  lst <- rl
  let arr = listArray (0,n) . scanl' (+) 0 . map myLog $ lst
  B.getContents >>= mapM_ (\x -> do
                              let [l,r] = map (fst . fromJust . B.readInt) $ B.words x
                              putStrLn . (\x -> if x then "Mishki" else "Hacker") $ calc l r arr
                          ) . B.lines
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
#include <stdio.h>
#include <stdlib.h>

typedef long int num_t;
static inline num_t myLog(num_t n) {
    num_t res = 0;
    for (int i = 0; n != 0; n = n / 2) res ++;
    return res;
}
static inline int calc(num_t l, num_t r, num_t *arr) {
    if ((arr[r] - arr[l-1]) % 2 == 1) return 1;
    return 0;
}
int main() {
    num_t n,q,k,*l,ll,rr;
    scanf("%ld%ld", &n, &q);
    l = malloc(sizeof(num_t) * n+1);
    l[0] = 0;
    for (int i = 1; i <= n; i++) {
        scanf("%ld", &k);
        l[i] = l[i-1]+myLog(k);
    }
    for (int i = 0; i < q; i++) {
        scanf("%ld%ld", &ll, &rr);
        if (calc(ll,rr,l))
            printf("Mishki\n");
        else
            printf("Hacker\n");
    }
}
-}
-- Longest Common Prefix
{-
import Data.List
lcpLen :: String -> String -> Int
lcpLen [] _ = 0
lcpLen _ [] = 0
lcpLen (x:xs) (y:ys)
  | x /= y = 0
  | otherwise = 1 + lcpLen xs ys
genSubStr :: String -> [String]
genSubStr [x] = [[x]]
genSubStr (x:xs) = map (x:) (inits xs) ++ genSubStr xs

genSubStrPair :: String -> [(String,String)]
genSubStrPair s = [ (x,y) | x <- genSubStr s, y <- genSubStr s ]

calc s = sum [ lcpLen x y | (x,y) <- genSubStrPair s ]
    
main = getLine >> getContents >>=
  mapM_ (putStrLn . show) . go . lines
  where go [] = []
        go (x:y:xs) = calc y : go xs
-}
-- Fredo and Large Numbers
{-
import qualified Data.HashTable as HT
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import qualified Data.Map.Strict as MM
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import Data.Ord
import Data.Function
main = do
  [n] <- rl'
  l <- rl
  [q] <- rl'
  let freqMap = genMap l
      rangeMap = genRangeMap l
  flip mapM_ [1..q] $ \_ -> do
    [t,f] <- rl'
    let exactMatch = M.lookup f freqMap
        leastMatch = M.lookupGE f rangeMap
    case t of
      0 -> if isJust leastMatch then putStrLn . show . snd $ (fromJust leastMatch)
                      else putStrLn "0"
      1 -> if isJust exactMatch then putStrLn . show $(fromJust exactMatch)
                      else putStrLn "0"
  where rl = fmap (map (fst . fromJust . B.readInteger) . B.words) B.getLine
        rl' = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
genMap :: [Integer] -> M.IntMap Integer
genMap l = M.fromListWith add (calc l)
  where add :: Integer -> Integer -> Integer
        add x 0 = x
        add _ old = old
genRangeMap :: [Integer] -> M.IntMap Integer
genRangeMap l = M.fromListWith add . myFilter 0  $ calc l
  where add :: Integer -> Integer -> Integer
        add x 0 = x
        add _ old = old
        myFilter :: Int -> [(Int, Integer)] -> [(Int, Integer)]
        myFilter _ [] = [] 
        myFilter maxk ((k,v):xs)
          | k > maxk  = (k,v) : myFilter k xs
          | otherwise = myFilter maxk xs
calc :: [Integer] -> [(Int, Integer)]
calc l = map snd . sortBy (comparing fst) . map go . groupBy ((==) `on` fst) . sortBy (comparing fst) $ zip l [0..]
  where go :: [(Integer, Int)] -> (Int, (Int, Integer)) -- (pos, (frq, value))
        go l = let (v,pos) = minimumBy (comparing snd) l
               in (pos, (fromIntegral $ length l, v))
test = genMap [1,2,2,1,2,3]
test2 = genMap [1,2,2,1,2,3,4]
test3 = genMap [1,2,2,1,2,4, 3]

-}
-- Fun With Strings

-- formula 1 calculates sum of lcp between two triangle m, n.
{-
import Data.List
big :: Int
big = 10^9+7

formula1:: Int -> Int -> Int -> Int
formula1 m n p =
  ((p*(n-q) + (p*q`div`2)) * (m-q) +
  p*q*(3*n-p+2)`div`6) `mod` big
  where q = p-1

diagonal :: Int -> Int
diagonal n = sum . map (\x -> formula1 x x x) $ [1..n]

solve, half :: String -> Int
half zs = sum [ f x a y b| (x,a) <- zip [l,l-1..] (init $ tails zs), let ll = length a,
                           (y,b) <- zip [ll-1,ll2..] (init (tail $ tails a)) ]
  where f lx x ly y = let p = lcpLen x y
                      in (formula1 lx ly p) `mod` big
        l = length zs

h zs = [ (f x y,(x,y))| x <- (init $ tails zs), y <- (init $ tail $ inits x) ]
  where f x y = let p = lcpLen x y
                    f = fromIntegral
                in (formula1 (f $ length x) (f $ length y) $ f p) `mod` big

solve zs = (half zs * 2 + diagonal (fromIntegral $ length zs)) `mod` big

lcpLen :: String -> String -> Int
lcpLen [] _ = 0
lcpLen _ [] = 0
lcpLen (x:xs) (y:ys)
  | x /= y = 0
  | otherwise = 1 + lcpLen xs ys

main = getLine >> getContents >>=
  mapM_ (putStrLn . show) . go . lines
  where go [] = []
        go (x:y:xs) = solve y : go xs
-}

-- Huge Sum
{-
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import Debug.Trace
big = 10^9+7
fac :: Int -> [Int]
fac n = map (length) . group $ factorize 2 n

factorize :: Int -> Int -> [Int]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

g :: Int -> Int -> Int
g 1 k = 1
g n k = myProduct (map (\x -> k*x + 1) u)
  where u = fac n
        myProduct [] = 1
        myProduct (x:xs) = (x * myProduct xs) `mod` big

f :: [Int] -> Int
f [n,k] = sum [ (g i k) `mod` big | i <- [1..n] ] `mod` big

main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . show . f . map (fst . fromJust . B.readInt) . B.words) . B.lines
-}
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import Debug.Trace
big = 10^9+7
fac :: Integer -> [Integer]
fac n = map (fromIntegral . length) . group $ factorize 2 n

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

g :: Integer -> Integer -> Integer
g 1 k = 1
g n k = myProduct (map (\x -> k*x + 1) u)
  where u = fac n
        myProduct [] = 1
        myProduct (x:xs) = (x * myProduct xs) `mod` big

f :: [Integer] -> Integer
f [n,k] = sum [ (g i k) `mod` big | i <- [1..n] ] `mod` big

main = B.getLine >> B.getContents >>=
  (mapM_ (putStrLn . show . f . map (fst . fromJust . B.readInteger) . B.words) . B.lines)
  >> putStrLn ""
