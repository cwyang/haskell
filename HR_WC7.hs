-- Gridland metro
{-
import qualified Data.IntMap as M
import Data.List
import Data.Ord
import Debug.Trace
rangeSum :: Int -> (Int,Int) -> [(Int,Int)] -> Int
rangeSum acc (a,b) []
  | trace (show acc ++ " " ++ show (a,b) ++ "!") False = undefined
  | otherwise = acc + (b-a+1)
rangeSum acc (a,b) f@((x,y):xs)
  | trace (show acc ++ " " ++ show (a,b) ++ " " ++ show f) False = undefined
  | b > y = rangeSum acc (a,b) xs
  | b <  x = rangeSum (acc + b-a+1) (x,y) xs
  | b <= y = rangeSum acc (a,y) xs

main = do
  [n,m,k] <- rl
  let mm = M.empty
  mm <- loop k M.empty 
  let v = sum . map (rangeSum 0 (0,-1) . sortBy (comparing fst)) $ M.elems mm
  print v
  print n
  print m
  
  print $ n*m - v
  where rl = fmap (map (read :: String->Int) . words) getLine
        loop 0 mm = return mm
        loop k mm = do
          [r,c1,c2] <- rl
          let mm' = case M.lookup r mm of
                Nothing -> M.insert r [(c1,c2)] mm
                Just xs -> M.insert r ((c1,c2):xs) mm
          loop (k-1) mm'


test = rangeSum 0 (0,0) [(0,1), (0,10)]
-}
-- Two Chars
{-
import Data.List

s = ['a'..'z']
calc :: String -> Int
calc [_] = 1
calc xs = let a =  [ length vs  | x <- s, y <- s, x /= y,
                     let vs = filter (\z -> z == x || z == y) xs, alter vs ]
          in if null a then 0 else maximum a

alter :: String -> Bool
alter [] = True
alter (x:xs) = alter' x xs
  where alter' _ [] = True
        alter' x (y:ys)
          | y == x = False
          | otherwise = alter' y ys

main = getLine >>= (print . calc)

test = calc "beabeefeab"
-}
-- Similar Strings
{-
substr s l r = take (r-l+1) $ drop (l-1) s

substrs s n = substrs' s n (length s - n + 1)
substrs' _ _ 0 = []
substrs' s n k = take n s : substrs' (tail s) n (k-1)

similar :: String -> String -> [String]
similar src pat = [ x | x <- substrs src l, check (zip x pat) ]
  where l = length pat

check :: [(Char,Char)] -> Bool
check [] = True
check ((x,y):xs) = check' (x,y) xs && check xs
  where check' (x,y) xs = all id [ (x == x1 && y == y1) || (x /= x1) && (y /= y1) | (x1,y1) <- xs ]

main = do
  [n,q] <- rl
  l <- getLine
  loop q l
  where rl = fmap (map (read :: String->Int) . words) getLine
        loop 0 _ = return ()
        loop q s = do
          [l,r] <- rl
          let s' = substr s l r
              n = length $ similar s s' 
          print n
          loop (q-1) s
-}
-- Summing Piece
{-
import Debug.Trace
big = 10^9+7
f :: Int -> Int -> Int -> [(Int, Int)]
f n k w = go n l
  where l = max 0 (k-w+1)
        go n l
          | l > k     = []
          | l + w > n = []
          | otherwise = (l, n-l-w) : go n (l+1)

f' :: Int -> Int -> Int -> (Int,Int,Int,Int)
f' n k w = (l, n-l-w,  (r-w+1), (n-r-1))
  where l = max 0 (k-w+1)
        r = min (k+w-1) (n-1)

g n k w = w * go a b c d
  where xs = f n k w
        (a,b,c,d) = f' n k w
        g' 0 = 1
        g' x = expModP 2 (x-1) big
        go a b c d
          | a > c            = 0
          | a == 0 && b == 0 = 1
          | a == 0           = g' b + go (a+1) (b-1) c d
          | d == 0           = g' c + go a b (c-1) (d+1)
          | otherwise        = ((c-a+1) * ((g' a * g' b) `mod` big)) `mod` big
--          | otherwise = sum $ map (\(l,r) -> g' l * g' r * w) xs

gg n k w = (sum $ map (\(l,r) -> g' l * g' r * w) xs) `mod` big
  where xs = f n k w
        g' 0 = 1
        g' x = expModP 2 (x-1) big
expModP :: Int -> Int -> Int -> Int -- a^k mod p
expModP _ 0 _ = 1
expModP a k p
  | odd k     = (a * expModP a (k-1) p) `rem` p
  | otherwise = expModP (a^2 `rem` p) (k `div` 2) p 

calc :: Int -> [Int] -> Int
calc n xs = sum [ g n p w * x | (x,p) <- zip xs [0..], w <- [1..n] ]

main = do
  [n] <- rl
  l <- rl
  print $ calc n l
  where rl = fmap (map (read :: String->Int) . words) getLine

foo = calc 3 [1,3,6]
test = calc 5 [4,2,9,10,1]
a = [ g 4 x y | x <- [0..3], y <- [1..4]]
b = [ gg 4 x y | x <- [0..3], y <- [1..4]]
-}
-- Elastic Rope
{-
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
type Point = (Double,Double)
calc :: Bool -> [Point] -> [Point] -> [Point]
calc _ acc []   = reverse acc
calc _ [] [p]   = [p]
calc _ [] [p,q] = [p,q]
calc flag (a:as) [p,q]
  | isHull flag a p q = calc flag as [a,q]
  | otherwise    = calc flag (q:p:a:as) []
calc flag [] (p:q:r:xs)
  | isHull flag p q r = calc flag [] (p:r:xs)
  | otherwise    = calc flag [p] (q:r:xs)
calc flag (a:as) (p:q:r:xs)
  | isHull flag p q r = calc flag as (a:p:r:xs)
  | otherwise    = calc flag (p:a:as) (q:r:xs)

isHull :: Bool -> Point -> Point -> Point -> Bool
isHull orientation (a,b) (c,d) (e,f)
  | ux*vy - uy*vx > 0 && orientation  = True
  | ux*vy - uy*vx <= 0 && (not orientation)  = True
  | otherwise         = False
  where (ux,uy) = (c-a,d-b)
        (vx,vy) = (e-c,f-d)
myLength :: [Point] -> Double
myLength [] = 0
myLength [_] = 0
myLength (p:q:xs) = dist p q + myLength (q:xs)

-- https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [Point] -> Bool
shoelace l
  | v < 0  = False -- Counter-clockwise
  | otherwise     = True  -- Clockwise
  where l' = zip l (tail l)
        v = sum (map (\((x1,y1),(x2,y2)) -> (x2-x1)*(y2+y1)) l')

foo = calc True [] [(200,100), (200,200), (100,200)]
foo' = calc False [] [(200,100), (200,200), (100,200)]
bar = calc True [] $reverse [(200,100), (200,200), (100,200)]

a',a,b :: [Point]
a = [(100, 100),
     (200, 100),
     (200, 200),
     (100, 200)]
b = [(167, 84),
     (421, 84),
     (283, 192),
     (433, 298),
     (164, 275),
     (320, 133)]

main = do
  [n,a,b] <- rl
  l <- readLoop n
  let b' = if a <= b then b else b+n
      a' = if b <= a then a else a+n
      s = take (b'-a+1) . drop (a-1) $ l++l
      s' = take (a'-b+1) . drop (b-1) $ l++l
      orientation = shoelace $ l ++ [head l]
      v1 = myLength . calc orientation [] $ s
      v2 = myLength . calc orientation [] $ s'
  print $ max v1 v2

  where rl = fmap (map (read :: String->Int) . words) getLine
        f = fromIntegral
        readLoop 0 = return []
        readLoop n = do
          [x,y] <- rl
          fmap ((f x,f y):) $ readLoop (n-1)
-}
-- Recording Episodes
import Data.List
import Debug.Trace
import Data.Ord
import Data.Monoid
ge x (y,z) = x >= y && x >= z
hmm xs = concat [ calc p [] x | (x,p) <- zip (tails xs) [1..] ]
calc :: Int -> [(Int,Int)] -> [[Int]] -> [[Int]]
calc n acc [] = [[]]
calc n acc ([a,b,c,d]:xs)
  | u && v = map (n:) (calc (n+1) ((a,b):acc) xs)
                        ++ map (n:) (calc (n+1) ((c,d):acc) xs)
  | u      = map (n:) (calc (n+1) ((a,b):acc) xs)
  | v      = map (n:) (calc (n+1) ((c,d):acc) xs)
  | otherwise = [[]] 
  where u = possible (a,b) acc
        v = possible (c,d) acc
possible :: (Int,Int) -> [(Int,Int)] -> Bool
possible (a,b) xs = all id $ map (\(x,y) -> nooverlap (a,b) (x,y)) xs
  where nooverlap (a,b) (c,d)
          | b < c = True
          | d < a = True
          | otherwise = False
main = do
  [t] <- rl
  loop t
  where rl = fmap (map (read :: String->Int) . words) getLine
        loop 0 = return ()
        loop k = do
          [n] <- rl
          l <- mapM (\_ -> rl) [1..n]
          putStrLn . unwords . map show . (\x -> [head x, last x]) . head . filter (/= []) . reverse . sortBy (comparing length <> flip compare) $ hmm l
          loop (k-1)

a,b,c,d :: [[Int]]
a = [[11, 19, 31, 39],
     [12, 38, 13, 37],
     [10, 20, 30, 40]]
b = [[10, 20, 30, 40],
     [20, 35, 21, 35],
     [14, 30, 35, 50]]
c = [[1,100,2,100],
     [2,100,3,100],
     [200,300,201,301],
     [300,500,300,700]]
d = [[1,100,1,100],
     [2,1100,2,2100],
     [300,500,400,600]]
test = sortBy (comparing length <> flip compare) [[1],[2,3,4],[1,2,3],[1,2]]
