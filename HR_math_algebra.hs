-- Stepping Stone
main = getLine >> getContents >>=
  mapM_ (putStrLn . tt . calc . read) . lines
  where calc :: Int -> Bool
        calc n = let x = round ((sqrt (1+8*fromIntegral n) - 1) / 2)
                 in x * (x+1) == 2 * n
        tt False = "Better Luck Next Time"
        tt _     = "Go On Bob"
          
{-
-- Wet Shark and 42

calc :: Int -> Int
calc k = d * 42 + r * 2
  where (d,r) = k `divMod` 20

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . calc . read) . lines
-}
