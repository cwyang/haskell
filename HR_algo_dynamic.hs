{-
-- The Maximum Subarray
import Text.Printf

-- mss by R.Bird
mss = maximum . scanr (@@) 0
  where x @@ y = 0 `max` (x+y)
go :: [Int] -> (Int, Int)
go l = (mss l, sum $ filter (> 0) l)

main = getLine >> getContents >>=
  mapM_ (putStrLn . (\(x,y) -> printf "%d %d" x y) . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-}
{-
-- Nikita and the game
import Data.Maybe
breaks :: Int -> [Int] -> Maybe ([Int], [Int])
breaks n [] = Just ([],[])
breaks 0 xs = Just ([],xs)
breaks n (x:xs)
  | n < x  = Nothing
  | otherwise = case breaks (n-x) xs of
      Nothing -> Nothing
      Just (a,b) -> Just (x:a,b)

go :: [Int] -> Int
go x
  | [_] <- x     = 0
  | odd s        = 0
  | t == Nothing = 0
  | l == []      = go (tail m) + 1
  | otherwise    = (max lv rv) + 1
  where s = sum x
        s' = s `div` 2
        t = breaks s' x
        (l,x') = fromJust t
        (m,r) = span (== 0) x'
        lv = go $ l ++ m
        rv = go $ m ++ r

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-}
{-
-- Coin Change
import Debug.Trace
import Data.Array
-- calc recursive
calc, solve :: Int -> Int -> [Int] -> Int
calc 0 _ _ = 1
calc _ 0 _ = 0
calc n 1 [x]
  | n < x          = 0
  | n `rem` x /= 0 = 0
  | otherwise      = 1
calc n m (x:xs)
    | otherwise= calc n (m-1) xs + sum [ calc (n - y) (m-1) xs | y <- [x,x*2..n]]
-- solve dynamic
solve n m l = table ! (n,m)
  where table :: Array (Int,Int) Int
        table = array ((0,0),(n,m)) [ ((i,j), f i j x) | j <- [0..m], i <- [0..n], let x = head $ drop (j-1) l]
        f i j x -- | trace (show i ++ " " ++ show j) False = undefined
                | i == 0 = 1
                | j == 0 = 0
                | j == 1 && i < x = 0
                | j == 1 && i `rem` x /= 0 = 0
                | j == 1 = 1
                | otherwise = table ! (i,j-1) + sum [ table ! (i-y,j-1) | y <- [x,x*2..i]]

main = do
  [n,m] <- rl
  l <- rl
  print $ solve n m l
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
{-
-- Red John
memoized_f, f :: Int -> Integer
f 0 = 1
f 1 = 1
f 2 = 1
f 3 = 1
f 4 = 2
f n = memoized_f (n-1) + memoized_f (n-4)
memoized_f = ((map f [0..]) !!)

numberPrimes n = length . takeWhile ( <= n) $ primes
primes :: [Integer]
primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . go . read) . lines
  where go :: Int -> Int
        go = numberPrimes . memoized_f
-}
-- Stock Maximize
{-
Splitlist l = let m = maximum l
                  (a,b) = break (==m) (reverse l)
              in (reverse b, reverse a)
  
maximize :: [Int] -> Int
maximize [] = 0
maximize [_] = 0
maximize l = let (a,b) = splitList l in
                 go 0 0 a + maximize b
  where
    go n acc [x]    = n*x - acc
    go n acc (x:xs) = go (n+1) (acc+x) xs
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . maximize) . doArg . lines
  where doArg [] = []
        doArg (_:x:xs) = rl x:doArg xs
        rl x = map read . words $ x
-}
-- Candies
{-
import Data.Array

candies :: Int -> [Int] -> Int
candies n ratings = go 0
  where bounds = (0,n-1)
        xarr = listArray bounds ratings
        arr = listArray bounds [go i | i <- bounds]
        go x
          | x == n-1  = (1,1)
          | xarr!x > xarr!x+1 = (tot+cur+1,cur+1)
          | cur > 1           = undefined
          where (tot,cur) = arr!(x+1)
-}
-- Knapsack
{-
import Data.Array
import Debug.Trace
knapsack, knapsack' :: [Int] -> Int -> Int
knapsack xs k = dp k 0
  where n = length xs
        bounds = (0,n-1)
        xarr = listArray bounds xs
        dp :: Int -> Int -> Int
        dp 0 _ = 0
        dp k i
--          | trace (show (k,i)) False = undefined
          | k <= 0    = 0
          | i == n    = 0
          | k >= v    = maximum $ (dp (k-v) i): map (dp k) [i+1..n-1]
          | i == n-1  = 0
          | otherwise = maximum $ map (dp k) [i+1..n-1]            
          where v = xarr ! i
        bounds2 = ((0,0),(k,n))
        arr = listArray bounds2 [ dp i j | (i,j) <- range bounds2 ]

-- dp k holds closest number to k. we need to find dp k
knapsack' xs k = dp k
  where dp 0 = 0
        dp x = safeMax [ i + arr!(x-i) | i <- xs, x >= i ]
        safeMax x = if null x then 0 else maximum x
        bounds = (0,k)
        arr = listArray bounds [ dp i | i <- range bounds ]

main = getLine >> getContents >>=
  mapM_ (putStrLn. show . uncurry knapsack) . chunk . lines
  where chunk [] = []
        chunk (a:b:xs) = let [n,k] = rl a
                             l     = rl b
                         in (l,k):chunk xs
        rl = map read . words
-}
-- Bricks Game
{-
import Data.Array
import Debug.Trace
solve' :: [Int] -> Int
solve' xs = dp 0
  where n = length xs
        xarr = listArray (0,n-1) xs
        dp k
          | k >= n   = 0
          | k >= n-3 = sum [ xarr!i | i <- [k..n-1]]
          | otherwise = maximum [ mySum k j + minimum [elem (k+i+j) | i <- [1..3]]
                                | j <- [1..3]]
        mySum base k = sum [ xarr!i | i <- [base..base+k-1] ]
        arr = listArray (0,n) [dp i | i <- [0..n] ]
        elem x | x >= n = 0
               | otherwise = arr!x

solve :: [Int] -> Int
solve xs = dp (n-1)
  where n = length xs
        xarr = listArray (0,n-1) $ reverse xs
        sumarr = listArray (0,n-1) $ scanl1 (+) $ reverse xs
        dp k
          | k < 3   = mySum k 3
          | otherwise = maximum [ mySum k 1 + sumarr!(k-1) - elem (k-1)
                                , mySum k 2 + sumarr!(k-2) - elem (k-2)
                                , mySum k 3 + sumarr!(k-3) - elem (k-3)]
        mySum base k = sum [ xarr!i | i <- [base,base-1..base-k+1], i >= 0 ]
        arr = listArray (0,n-1) [dp i | i <- [0..n] ]
        elem x | x >= n = 0
               | otherwise = arr!x

main = getLine >> getContents >>=
  mapM_ (putStrLn. show . solve) . chunk . lines
  where chunk [] = []
        chunk (a:b:xs) = let [n] = rl a
                             l     = rl b
                         in (l):chunk xs
        rl = map read . words
-}
-- Sam and Substring
import Data.Array
big = 10^9+7
solve :: [Int] -> Int
solve xs = sum lst
  where dp k
          | k < 0 = 0
          | otherwise = (elem (k-1) * 10 + (xarr!k) * (k+1)) `mod` big
        n = length xs
        bounds = (0,n-1)
        xarr = listArray bounds xs
        arr = listArray bounds lst
        lst = [dp i | i <- range bounds]
        elem x | x < 0 = 0
               | otherwise = arr ! x
main = getLine >>= print . solve . map (read .return)
  
