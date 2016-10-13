{-
-- Jumping on the Cloud
go, calc :: Int -> [(Int,Int)] -> Int -> Int
go acc l@((v,p):_) k = calc (acc-1) (drop k l) k
calc acc l@((v,p):_) k
  | p == 0 && v == 1 = acc - 2
  | p == 0           = acc
  | v == 1    = calc (acc-3) (drop k l) k
  | otherwise = calc (acc-1) (drop k l) k
main = do
  [n,k] <- rl
  l <- rl
  let foo = cycle (zip l [0::Int,1..])
  print $ go 100 foo k
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
