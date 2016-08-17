{-
-- CamelCase
import Data.Char
main = getLine >>= print . succ . length . filter isUpper

-- String Construction
import Data.List
main = getLine >> getContents >>= mapM_ (print . length . group . sort) . lines
-}

{-
-- Short Palindrome

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Data.Char
choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

num :: V.Vector Int -> Int -> [Int] -> Int
num _ _ [] = 0
num _ _ [_] = 0
num _ _ [_,_] = 0
num _ _ [_,_,_] = 0
num m le x
  | head x /= le = 0
  | otherwise = sum . map go . zip [0..] $ V.toList m
  where go :: (Int, Int) -> Int
        go (a,b)
          | b' < 2 = 0
          | otherwise = (choose b' 2) `rem` (10^9+7)
          where b' = if a == head x then b-2 else b

scan :: V.Vector Int -> Int -> [Int] -> Int
scan m _ [] = 0
scan m le x@(xh:xs) = (num m le x + scan m' le xs) `rem` (10^9+7)
  where oldVal = m ! xh
        m' = m // [(xh, oldVal - 1)]

calc :: V.Vector Int -> [Int] -> [Int] -> Int
calc m _ [] =  0
calc m prefix x@(xh:xs) = (scan m' xh (reverse prefix') + calc m' prefix' xs) `rem` (10^9+7)
  where oldVal = m ! xh
        m' = m // [(xh, oldVal + 1)]
        prefix' = xh:prefix

main = do
  s <- getLine
  let l = map (\x -> ord(x) - ord('a')) s
      v = calc (V.fromListN 26 (repeat 0)) [] l
  print $ v `rem` (10^9+7)
-}

-- Bulid a Palindrome
{-# OPTIONS_GHC -O3 #-}
import Data.Monoid
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as B
allSubstring :: B.ByteString -> [B.ByteString]
allSubstring = concatMap (tail . B.inits) . B.tails
getCommon :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString, Bool)
getCommon a b
  | B.length a > B.length b  = getCommon b a
  | B.length a == B.length b = if a == b then (a, B.empty, True) else (B.empty, B.empty, False)
  | a == (B.take (B.length a) b) && isPalindrome r = (a, r, True)
  | otherwise                = (B.empty, B.empty, False)
  where r = B.drop (B.length a) b
genStr :: B.ByteString -> B.ByteString -> [B.ByteString]
genStr a b = [ B.concat [common, remain, B.reverse common] | x <- a', x /= B.empty ,
                                                             let b' = B.dropWhile (\a -> a /= B.head x) b,
                                                             y <- allSubstring b', y /= B.empty,
                              let (common, remain, res) = getCommon x y, res == True
                            ]
  where a' = allSubstring a
isPalindrome :: B.ByteString -> Bool
isPalindrome a = a == B.reverse a
calc :: [B.ByteString] -> String
calc [a,b] = if null res then "-1"
           else B.unpack $ head res
  where res = sortBy (flip (comparing B.length) <> compare) . filter isPalindrome $ genStr a (B.reverse b)
main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . calc) . doArg . B.lines
  where doArg [] = []
        doArg (x:y:xs) = [x,y]:doArg xs
