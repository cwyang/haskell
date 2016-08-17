-- Birthday Cake Candles
{-
import Data.List
main = do
  rl
  l <- rl
  print $ length . head . group . reverse . sort $ l
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- Counting Valleys
{-
import Data.List
calc :: [Int] -> Int
calc = snd . foldl' go (0,0) -- (Last, ValleyCount)
go :: (Int,Int) -> Int -> (Int,Int)
go (last,cnt) x
  | last == -1 && x == 1 = (last+1, cnt+1)
  | x == 1               = (last+1, cnt)
  | otherwise            = (last-1, cnt)
main = do
  n <- readLn :: IO Int
  l' <- getLine
  let l = map (\x -> if x == 'U' then 1 else -1) l'
  print $ calc l
-}
-- Magic Square Forming
{-
import Data.List
perm :: Eq a => [a] -> [[a]]
perm [x] = [[x]]
perm xs = concatMap (\x -> map (x:) $ perm (delete x xs)) xs

transpose' :: [[a]] -> [[a]]
transpose' ([]:_) = []
transpose' x = (map head x): transpose' (map tail x)

split :: [a] -> Int -> [[a]]
split [] _  = []
split l n = take n l : split (drop n l) n
magic :: [[Int]]
magic = [ x | x <- perm [1..9],
          let y@[a@[a1,_,a3],b@[_,b2,_],c@[c1,_,c3]] = split x 3,
              map sum y == [15,15,15],
              map sum (transpose y) == [15,15,15],
              a1+b2+c3 == 15,
              a3+b2+c1 == 15]
magic'=[[2,7,6,9,5,1,4,3,8],[2,9,4,7,5,3,6,1,8],[4,3,8,9,5,1,2,7,6],[4,9,2,3,5,7,8,1,6],[6,1,8,7,5,3,2,9,4],[6,7,2,1,5,9,8,3,4],[8,1,6,3,5,7,4,9,2],[8,3,4,1,5,9,6,7,2]]

main = do
  l1 <- rl
  l2 <- rl
  l3 <- rl
  let l = l1++l2++l3
  print . head . sort . map (diff l) $ magic'
  where rl = fmap (map (read :: String->Int) . words) getLine
        diff a b = sum $ zipWith (\x y -> abs (x-y)) a b
-}
-- Extremely Dangerous Virus
{-
expModP :: Int -> Int -> Int -> Int -- a^k mod p
expModP _ 0 _ = 1
expModP a k p
  | odd k     = (a * expModP a (k-1) p) `rem` p
  | otherwise = expModP (a^2 `rem` p) (k `div` 2) p 
inv :: Int -> Int -> Int
inv k p = expModP k (p-2) p
p :: Int
p = 10^9+7
main = do
  [a,b,t] <- rl
  let inv2 = inv 2 p
      x = expModP ((a+b) * inv2) t p
  print x
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- AntiPrime Numbers
