-- Wet Shark and 42

calc :: Int -> Int
calc k = d * 42 + r * 2
  where (d,r) = k `divMod` 20

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . calc . read) . lines
