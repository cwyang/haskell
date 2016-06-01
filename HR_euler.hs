-- #81 Path sum


pathSum :: [Int] -> [[Int]] -> Int
pathSum x [] = last x
pathSum x (y:ys) = pathSum x' ys
  where x' = reverse . fst $ foldl go ([],head x) (zip x y)
        go (res, las) (x,y) = if x < las
                              then (x+y : res, x+y)
                              else (las+y : res, las+y)

main = do
  n <- readLn :: IO Int
  l <- sequence $ replicate n $ (map (read :: String->Int) . words) <$> getLine
  print $ pathSum (scanl1 (+) (head l)) (tail l)
                 
