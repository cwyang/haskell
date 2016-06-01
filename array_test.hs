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
