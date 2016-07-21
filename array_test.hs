import Data.Array
import Data.List
import Control.DeepSeq

go :: [Int] -> [Int]
go = elems . foldl' update (array (0,99) [(i,0) | i <- [0..99]])
  where update acc x = let a' = acc // [(x, acc ! x+1)]
                       in bounds a' `seq` map (\x -> x `seq` ()) (elems a') `seq` elems a' `seq` a'
--                       in a' `deepseq` a'

main = putStrLn . unwords . map show . go . concat . replicate 5000 $ [1..99]
foo  = putStrLn . unwords . map show . go . concat . replicate 5000 $ [1..99]

bar = accumArray (+) 0 (0,10) [(x `rem` 10,1) | x <- [1..10]]
baz k = accumArray (+) 0 ((1,1),(k,k)) [(p,n) | n <- [1..(k*2-1)],
                                                        let start = if n > k then n-k+1 else 1
                                                            end   = if n > k then k else n,
                                                        x <- [start..end],
                                                        let p = (x,n-x+1)]
