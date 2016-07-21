{-
-- Sherlock and Valid String
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord

buildMap :: [Char] -> [Int] 
buildMap = map snd . M.toList . foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

decide :: [Char] -> Bool
decide s
  | same m = True
  | head m == head (tail m) + 1 && same (tail m) = True
  | last m == 1 && same (init m) = True
  | otherwise = False
  where m = sortBy (flip compare) . buildMap $ s
        same x = all (== head x) (tail x)

main = getLine >>= (putStrLn . (\x -> if x then "YES" else "NO") . decide)                            
--
import Data.List

getExactN :: Int -> String -> [String]
getExactN _ [] = []
getExactN k x@(_:xs)
  | length v < k = []
  | otherwise = v : getExactN k xs
  where v = take k x
        
tuplify :: [a] -> [(a,a)]
tuplify (x:xs) = map ((,) x) xs

getPairs :: String -> [(String, String)]
getPairs x = concatMap go (tails x)
  where go x = concatMap (\k -> tuplify $ getExactN k x) (enumFromTo 1 (length x))

isAnagram :: String -> String -> Bool
isAnagram x y = sort x == sort y

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . length . filter (uncurry isAnagram) . getPairs) .lines
-}
{-
-- String Transmission
foldK :: [Char] -> Int -> [[Char]]
foldK [] _ = [[]]
foldK x 0 = [x]
foldK (x:xs) k = map (switch x:) (foldK xs (k-1)) ++ map (x:) (foldK xs k)
  where switch '0' = '1'
        switch '1' = '0'

patMatch :: (Show a,Eq a) => [a] -> [a] -> [a] -> ([a],[a],[a]) -- matched, remain, patremain
patMatch pat [] p = ([], [], p)
patMatch pat xs [] = patMatch pat xs pat
patMatch pat xs@(x:xs') (p:ps)
  | x == p    = let (ys,zs,ws) = patMatch pat xs' ps in (x:ys, zs,ws)
  | otherwise = ([], xs, []) 
    
periodic :: (Show a, Eq a) => [a] -> [a] -> Bool
periodic pat xs
  | not $ null ws = False       -- abcab -> False
  | null zs = True              -- abab -> True
  | length zs == 1 = False      -- aba  -> false
  | otherwise = periodic (pat ++ ys ++ [head zs]) (tail zs)
  where (ys,zs,ws) = patMatch pat xs []
foldK :: [Char] -> Int -> [[Char]]
foldK [] _ = [[]]
foldK x 0 = [x]
foldK (x:xs) k = map (switch x:) (foldK xs (k-1)) ++ map (x:) (foldK xs k)
  where switch '0' = '1'
                switch '1' = '0'

                patMatch :: (Show a,Eq a) => [a] -> [a] -> [a] -> ([a],[a],[a]) -- matched, remain, patremain
patMatch pat [] p = ([], [], p)
patMatch pat xs [] = patMatch pat xs pat
patMatch pat xs@(x:xs') (p:ps)
  | x == p    = let (ys,zs,ws) = patMatch pat xs' ps in (x:ys, zs,ws)
  | otherwise = ([], xs, [])

periodic :: (Show a, Eq a) => [a] -> [a] -> Bool
periodic pat xs
  | not $ null ws = False       -- abcab -> False
  | null zs = True              -- abab -> True
  | length zs == 1 = False      -- aba  -> false
  | otherwise = periodic (pat ++ ys ++ [head zs]) (tail zs)
      where (ys,zs,ws) = patMatch pat xs []
            
go :: (Int,Int,String) -> Int
go (n,k,x) = let xs = foldK x k in
  length . filter (\x -> (length x == 1) || (not $ periodic [head x] (tail x))) $ xs
chunk :: [String] -> [(Int,Int,String)]
chunk [] = []
chunk (x:y:xs) = let [n,k] = map read . words $ x
                 in (n,k,y) : chunk xs
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . go) . chunk . lines
-}
{-
-- Bear gene
import Data.List

inc, del :: Eq a => a -> [(a,Int)] -> [(a,Int)]
inc x xs = case lookup x xs of
  Just v  -> (x,v+1):(delete (x,v) xs)
del x xs = case lookup x xs of
  Just v  -> (x,v-1):(delete (x,v) xs)
  
scan :: Eq a => [(a,Int)] -> Int -> [a] -> [(a,Int)]
scan res _ [] = res
scan res threshold (x:xs) = case lookup x res of
  Nothing -> scan ((x,1):res) threshold xs
  Just v  -> if v >= threshold then res
             else (inc x res) threshold xs

scan2 :: Eq a => Int -> [(a,Int)] -> Int -> [a] -> [a] -> Int
scan2 maxLength _ _ _ [] = maxLength
scan2 maxLength res threshold [] (x:xs) =
  let xv = lookup x res in
  case xv of
    Nothing -> scan2 (maxLength+1), ((x,1):res) threshold [] xs
    Just v  -> if v >= threshold then maxLength
               else (maxLength+1) (inc x res) threshold [] xs
scan2 maxLength res threshold (y:ys) (x:xs) =
  let xv = lookup x res
      yv = lookup y res in
  case xv of
    Nothing -> scan2 (maxLength+1), ((x,1):res) threshold (y:ys) xs
    Just v  -> if v >= threshold then scan2 maxLength res 
               else (maxLength+1) (inc x res) threshold y:ys xs


geneScan :: Eq a => [a] -> Int
geneScan xs = let len = length xs `div` 4
                  r = scan [] len xs
                  lr = reverse $ take (sum (map snd r)) xs
              in length xs - scan2 (length res) res len lr (reverse xs)
-}
-- Bear Gene 2nd try
