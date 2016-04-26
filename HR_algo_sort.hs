{-
-- Insertion sort procession
import Data.List

insSort :: Ord a => [a] -> [[a]]
insSort l = insSort' [] l

insSort' :: Ord a => [a] -> [a] -> [[a]]
insSort' sorted [] = [sorted]
insSort' sorted p@(x:xs) = (sorted++p):insSort' sorted' xs
  where (a,b) = partition (< x) sorted
        sorted' = a ++ (x:b)

main = getLine >> getContents >>=
      (putStrLn . unlines . map (unwords . map show) . tail . tail . insSort . map read . words)
-}
