genBST :: Int -> Integer
genBST n = sum $ map (\x -> bst' (x-1) * bst' (n-x)) [1..n]
  where bst' = (bstList !!)
bstList = 1:1:(map genBST [2..])

