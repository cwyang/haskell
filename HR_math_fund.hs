{-
-- Sumar and Floating Rocks
-- find the number of integer coordinates on hypotenuse
calc :: [Int] -> Int 
calc [a,b,c,d] = gcd w h - 1
  where (w,h) = (abs (a-c), abs(b-d))
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . calc) . map (map read . words). lines
-}
import Data.List
import Debug.Trace
isPrime n
-- | trace (show n) False = undefined
 | otherwise = n > 1 &&
 foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
   True primes
primes :: [Int]
primes = 2 : filter isPrime [3,5..]

numDivisors :: Int -> Int
numDivisors n = numDivisors' n primes
numDivisors' :: Int -> [Int] -> Int
numDivisors' n (k:ks)
--  | trace (">> " ++ show n ++ ":" ++ show k) False = undefined
  | n == 1    = 1
  | isPrime n = 2
  | r == 0    = (d+1)
  | otherwise = (d+1) * numDivisors' r ks
  where go n k
          | n `rem` k == 0 = let (d,r) = go (n `div` k) k in (d+1,r)
          | otherwise      = (0,n)
        (d,r) = go n k

divisors :: Int -> [Int]
divisors n = divisors' n primes
divisors' :: Int -> [Int] -> [Int]
divisors' n (k:ks)
--  | trace (show n ++ ":" ++ show k) False = undefined
  | n == 1    = [1]
  | isPrime n = [1,n]
  | r == 0    = [ x | x <- map (k^) [0..d] ]
  | otherwise = sort [ x*y | x <- map (k^) [0..d], y <- divisors' r ks ]
  where go n k
          | n `rem` k == 0 = let (d,r) = go (n `div` k) k in (d+1,r)
          | otherwise      = (0,n)
        (d,r) = go n k

numEvenDivisors :: Int -> Int
numEvenDivisors' = length . filter even . divisors
numEvenDivisors k
  | odd k = 0
  | otherwise = (numDivisors r) * d
  where
    go n k
      | n `rem` k == 0 = let (d,r) = go (n `div` k) k in (d+1,r)
      | otherwise      = (0,n)
    (d,r) = go k 2
    
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . numEvenDivisors . read) . lines
  
bar1 = divisors 2093736587
bar2 = divisors 211047202
--[1,2,47,94,2245183,4490366,105523601,211047202]
baz2 = numDivisors 211047202
-- :set +s
foo = gcds [15015, 6006, 10010, 48048,10920,17160,4290,73920 ]
