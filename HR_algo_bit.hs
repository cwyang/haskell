{-
-- Xor sequence
import Data.Bits
import Data.List

xorSeq n 
  | n `rem` 4 == 1 = 1
  | n `rem` 4 == 2 = n+1
  | n `rem` 4 == 3 = 0
  | n `rem` 4 == 0 = n

xorSum [] = 0
xorSum (x:xs) = x `xor` xorSum xs

xorSeq' n = foldl' (\acc x -> acc `xor` x) 0 [1..n]
  
xorSeqSum l r
  | l' < r' = xorSum . map xorSeq $ [l,l+1..l'-1] ++ [r',r'+1..r]
  | otherwise = xorSum . map xorSeq $ [l..r]
  where l' = (l+7) `div` 8 * 8
        r' = (r-1) `div` 8 * 8
-}
-- cipher
{- N=number of bit, K shifts, XOR
1234
 1234
  1234
7 4
1110100110 => 1001010
it's possible to N < K
12
   12
12012
-
-}
import Data.Bits
xorSum :: [Int] -> Int
xorSum [] = 0
xorSum (x:xs) = x `xor` xorSum xs

decode, prefix :: Int -> Int -> [Int] -> [Int]
decode n k l = prefix n k l ++ take (n-k) (zipWith3 xor3 l' (tail l') (decode n k l))
  where xor3 x y z = x `xor` y `xor` z
        l' = drop (k-1) l

prefix n k l = take n $ take k $ prefix' n k 0 l
  where prefix' _ _ _ [] = []
        prefix' n k prev (x:xs) = prev `xor` x : prefix' n k x xs
