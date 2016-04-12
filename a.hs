import Text.Printf

main = do
  mapM_ (putStrLn . printf "3 1 %d") ([1..300000] :: [Int])
  mapM_ (putStrLn . printf "3 1 %d") ([1..300000] :: [Int])
  putStrLn "1 1"
