{-
-- Euler's criterion
expModP :: Int -> Int -> Int -> Int -- a^k mod p
expModP _ 0 _ = 1
expModP a k p
  | odd k     = (a * expModP a (k-1) p) `rem` p
  | otherwise = expModP (a^2 `rem` p) (k `div` 2) p 
go :: [Int] -> String
go [a,m]
  | a == 0 = "YES"
  | m == 2 = "YES"
  | t == 1 = "YES"
  | otherwise = "NO"                
  where t = expModP a (div (m-1) 2) m
main = getLine >> getContents >>=
  mapM_ (putStrLn .  go) . doArg . lines
  where doArg [] = []
        doArg (y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-}
{-
-- closest number
go :: [Int] -> Int
go [a,b,x]
  | r == 0 = k
  | r <= (x `div` 2) = (d)*x
  | otherwise        = (d+1)*x
  where k = a**b
        (d,r) = divMod k x
main = getLine >> getContents >>=
  mapM_ (putStrLn . show .  go) . map rl . lines
  where rl = map (read :: String->Int) . words
-}
{-
-- Sherlock and GCD
import Data.List
gcds :: [Int] -> Int
gcds [] = 0
gcds x = foldr1 gcd x
solve :: [Int] -> Bool
solve l = gcds l == 1

tt :: Bool -> String
tt True = "YES"
tt _    = "NO"
main = getLine >> getContents >>=
  mapM_ (putStrLn . tt . solve . nub) . map rl . doArg . lines
  where rl = map (read :: String->Int) . words
        doArg [] = []
        doArg (_:x:xs) = x:doArg xs
-}

{-
-- Smith Number
-- A Smith number is a composite number, the sum of whose digits is the sum of the digits of its prime factors obtained as a result of prime factorization (excluding ).
import Data.Char

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2

sumDigit :: [Integer] -> Integer
sumDigit [] = 0
sumDigit (x:xs) = sum' x + sumDigit xs
  where sum' = fromIntegral. sum . map digitToInt . show 

isSmith :: Int -> Bool
isSmith n = v1 == v2
  where v1 = sumDigit . primeFactors . fromIntegral $ n
        v2 = fromIntegral . sum . map digitToInt . show $ n
main = do
  n <- readLn
  if isSmith n then print 1 else print 0
-}
{-
-- John and GCDS
john :: [Int] -> [Int]
john l = [head l] ++ zipWith lcd l (tail l) ++ [last l]
lcd a b = a' * b' * g
  where g = gcd a b
        a' = div a g
        b' = div b g
main = getLine >> getContents >>=
  mapM_ (putStrLn . unwords . map show . john) . map rl . doArg . lines
  where rl = map (read :: String->Int) . words
        doArg [] = []
        doArg (_:x:xs) = x:doArg xs
-}
-- Cheese and Random Topping
import Data.List

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = factorize 2

chooseModP :: Int -> Int -> Int -> Int
chooseModP n 0 _ = 1
chooseModP 0 k _ = 0
chooseModP n k p = (v * chooseModP (n-1) (k-1) p) `rem` p
  where v = ((n `rem` p) * inv k p) `rem` p
        inv :: Int -> Int -> Int
        inv k p = expModP k (p-2) p
        expModP :: Int -> Int -> Int -> Int -- a^k mod p
        expModP _ 0 _ = 1
        expModP a k p
          | odd k     = (a * expModP a (k-1) p) `rem` p
          | otherwise = expModP (a^2 `rem` p) (k `div` 2) p 

baseK :: Int -> Int -> [Int] -- reversed. baseK 4 2 = [0 0 1]
baseK n k
  | n < k = [n]
  | otherwise = r : baseK d k
  where (d,r) = divMod n k

choose :: Int -> Int -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * fromIntegral n `div` fromIntegral k

-- nCk mod p
lucas :: Int -> Int -> Int -> Int
lucas n k p
  | n < k = 0
  | otherwise = foldl' go 1 c
  where c = zip (baseK n p) (baseK k p ++ [0,0..])
        go acc (n,k) = acc * fromInteger (choose n k `rem` fromIntegral p)
-- compose using Chinese Remainder
solve :: [Int] -> Int
solve [n,k,m] = foldl' go 0 l `rem` m
  where l = primeFactors (fromIntegral m)
        go acc p
          | acc == 0  = v
          | otherwise = acc * v
          where
            p' = fromInteger p
            v = lucas n k p'

main = getLine >> getContents >>=
  mapM_ (putStrLn . show .  solve) . map rl . lines
  where rl = map (read :: String->Int) . words

