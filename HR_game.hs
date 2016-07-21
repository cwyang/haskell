{-
-- Game of Stones
stone, stone' :: Int -> Bool
stone' = (map stone [0..] !!) -- combinator form, CAF for memoization
stone n
  | n <= 1 = False
  | n <= 5 = True
  | stone' (n - 2) && stone' (n - 3) && stone' (n - 5) = False
  | otherwise = True

main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "First" else "Second") . stone') . map read . lines
-- A Chessboard Game
import Data.Array
import Data.Maybe

k :: Int -- chessboard size
k = 15

-- True Means that player wins
go' :: Array (Int,Int) Bool
go' = accumArray (||) False ((1,1),(k,k)) [(pos,val k pos) | n <- [1..(k*2-1)],
                                            let start = if n > k then n-k+1 else 1
                                                end   = if n > k then k else n,
                                                x <- [start..end],
                                                let pos = (x,n-x+1)]
-- ALL True then False, otherwise True.
val :: Int -> (Int,Int) -> Bool
val k (y,x) = not . all id . fromJust . sequence . filter isJust $
  [ v (x-2,y+1)
  , v (x-2,y-1)
  , v (x+1,y-2)
  , v (x-1,y-2)
  ]
  where v (x,y)
          | y < 1 || x < 1 || y > k || x > k = Nothing
          | otherwise = Just $ go' ! (x,y)

main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "First" else "Second") . (go' !)) . map readTuple . lines
  where readTuple = (\[x,y] -> (x,y)) . map (read :: String->Int) . words

-- Introduction to Nim Game
-- https://www.math.ucla.edu/~tom/Game_Theory/comb.pdf
import Data.Bits
go :: [Int] -> Bool -- True then first win
go x = foldl go' 0 x /= 0
  where go' acc x = acc `xor` x

main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "First" else "Second") . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-- Misere Nim
-- https://www.math.ucla.edu/~tom/Game_Theory/comb.pdf
-- Same with Normal Nim. But if initial condition has all 1-size piles,
-- odd/even number of piles determine winner
import Data.Bits
import Data.List
go :: [Int] -> Bool -- True then first win
go x
  | (head . reverse . sort) x == 1 = even $ length x
  | otherwise = foldl go' 0 x /= 0
  where go' acc x = acc `xor` x

main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "First" else "Second") . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-- Nimble -- How dumb I am!!
-- similar to Nim -- no exactly same

import Data.Bits
go :: [Int] -> Bool -- True then first win
go x = foldl go' 0 (zip [1..] (tail x)) /= 0
  where go' acc (k,x) = if even x then acc :: Int else acc `xor` k
        
main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "First" else "Second") . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-- Tower Breakers -- Oh! there is no need to check prime case -_-;;;
primes :: [Int]
primes'' = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
primes = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]
go :: [Int] -> Bool -- True then first win
go [n,m]
  | m == 1 = False
--  | isPrime m && even n = False
--  | isPrime m = True
  | even n = False
  | odd  n = True
isPrime x = let (_,v) = break (>= x) primes
            in if head v == x then True else False
main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "1" else "2") . go) . doArg . lines
  where doArg [] = []
        doArg (y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-- Tower Breakers Revisited
-- n = p^a*q^b*r^c...
-- then number of divisors are (a+1)*(b+1)..
-- ARGH! it was prime factors, not total number of divisors that to be computed
import Data.Bits
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Debug.Trace

isPrime n = n > 1 &&
 foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
   True primes
primes = 2 : filter isPrime [3,5..]

divisors :: Int -> [Int] -> Int
divisors n (k:ks)
  | n == 1   = 0
  | isPrime n = 1
  | n `rem` k == 0 = d + divisors r ks
  | otherwise      = divisors n ks
  where go n k
          | n `rem` k == 0 = let (d,r) = go (n `div` k) k in (d+1,r)
          | otherwise      = (0,n)
        (d,r) = go n k

convNim :: [Int] -> [Int]
convNim = map go
  where go x = divisors x primes
  
calc :: [Int] -> Bool -- True then first win
calc x = foldl go 0 x /= 0
  where go acc x = acc `xor` x

main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . (\x -> if x then "1" else "2") . calc . convNim) . doArg . B.lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (fst . fromJust . B.readInt) . B.words
-}
{-
-- Fun Game
import Data.List
import Debug.Trace
calc :: ([Int], [Int]) -> String
calc (as,bs)
  | trace ("->" ++ show rs ++ " " ++ show vs ++ " " ++ show (go rs vs)) False = undefined
  | otherwise = case signum (go 1 rs vs) of
  1 -> "First"
  0 -> "Tie"
  -1 -> "Second"
  where vs' = sort $ zipWith (\a b -> (a-b, negate a)) as bs
        go _ [] = 0
        go _ [x] = vala x
        go n x
          | odd x && va > vb
          | evenx =
            where 
        go (x:maxs) (y:mins) = vala x - valb y + go maxs mins
        vala (x,y) = negate y
        valb (x,y) = negate $ x + y
main = getLine >> getContents >>=
  mapM_ (putStrLn . calc) . doArg . lines
  where doArg [] = []
        doArg (_:y:z:xs) = (rl y, rl z) : doArg xs
        rl = map (read :: String->Int) . words

foo = calc ([1,1,2,2],[1,1,2,2]) -- tie
f1 = calc ([100000,2134,21344],[3, 12313, 33333])  -- first
f2 = calc ([2],[100000]) --first
f3 = calc ([10,20,30],[50,40,30]) -- tie
f4 = calc ([1, 6, 2, 7],[9, 11, 7, 3]) -- second
f5 = calc ([20, 2],[1, 10000]) -- first
-- Chocolate in a Box
-- == Nim game
import Data.Bits
go :: [Int] -> Int -- the number of ways for first player to win, that is how to make xor sum to zero
go x
  | v == 0    = 0
  | otherwise = length $ filter (flip testBit p) x
  where go' acc x = acc `xor` x
        v = foldl go' 0 x
        p = getMSBpos v - 1
        getMSBpos 0 = 0
        getMSBpos n = 1 + getMSBpos (shiftR n 1)
main =  getContents >>=
  mapM_ (putStrLn . show . go) . doArg . lines
  where doArg [] = []
        doArg (_:y:xs) = rl y : doArg xs
        rl = map (read :: String->Int) . words
-}
-- A Stones Game (== Nim game)
import Data.Bits
go :: Int -> Int -- the number of ways for first player to win, that is how to make xor sum to zero
go x
  | odd x = 1
  | otherwise = shiftL 1 (p-1) -1
  where p = getMSBpos x
        getMSBpos 0 = 0
        getMSBpos n = 1 + getMSBpos (shiftR n 1)
main =  getLine >> getContents >>=
  mapM_ (putStrLn . show . go . read) . lines
