{-
-- Match The Shoes
import Data.List
import Data.Array
import Data.Ord

sortItem :: Int -> Int -> Int -> [Int] -> [Int]
sortItem k m n l = take k . map fst . reverse . sortBy (comparing go) . assocs $
  accumArray (+) 0 (0,m-1) [ (x,1) | x <- l ]
  where go (pos, num) = (num, m - pos)

main = do
  [k,m,n] <- rl
  content <- getContents
  let l = map (read :: String->Int) . lines $ content
  putStrLn . unlines . map show $ sortItem k m n l
  where rl = fmap (map (read :: String->Int) . words) getLine
 
-- The Inquiring Manager
import qualified Data.IntMap.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe

process :: M.IntMap Int -> [[Int]] -> [Int]
process _ [] = []
process m (x@(cmd:arg):xs)
  | cmd == 1  = process (M.insertWith max time price m') xs
  | otherwise = getMaximum m' : process m' xs
  where time = if cmd == 1 then head (tail arg) else head arg
        price = head arg 
        m' = dropBefore (time-60) m
        dropBefore time m = snd $ M.split time m
        getMaximum m = let l = M.elems m
                       in if null l then -1 else maximum l
foo = process M.empty [[1,150,0],[1,3,10],[2,40]]
main = do
  B.getLine
  content <- B.getContents
  let l = map (map (fst . fromJust . B.readInt) . B.words) . B.lines $ content
  putStrLn . unlines . map show $ process M.empty $ l
-- Minimal Wrapping Surface Area
import Data.List
import Data.Ord
surface :: (Int,Int,Int) -> (Int,Int,Int) -> Int
surface (a,b,c) (x,y,z) = 2*(x1*y1 + y1*z1 + x1*z1)
  where (x1,y1,z1) = (a*x,b*y,c*z)
combination :: Int -> Int -> [(Int,Int,Int)]
combination m n = [ (x,y,z) | x <- [1..sqr3], y <- [x..sqr2], z <- [y..(n`div`(x * y))],
                    m <= x*y*z && x*y*z <= n ]
  where sqr2 = round . sqrt . fromIntegral $ n
        sqr3 = round . (**(1/3)) . fromIntegral $ n
solve :: Int -> Int -> Int -> [(Int,Int,Int)] -> Int
solve a b c l = fst . head . sortBy (comparing fst) $
  map (\(x,y,z) -> (surface (a,b,c) (x,y,z), (x,y,z))) l
main = do
  n <- readLn :: IO Int
  line <- getLine
  let [a,b,c] = reverse . sort . map (read :: String-> Int) . words $ line
      answer = solve a b c (combination n 1000)
  print answer
-- Which Warehouse -> Bin Packing (NP Complete, Brute Force)
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = [ x:y | y <- choose (n-1) xs ] ++ choose n xs
process :: Int -> Int -> [[Int]] -> [[Int]] -> [Int]
process _ _ _ [] = []
process w p lw (b:bs) = (if null v then -1 else head v) : process w p lw bs
  where v = [ x | x <- [1..w], y <- choose x lw, let z = agg y, z `include` b]
        include x y = all id $ zipWith (>=) x y
        agg = foldl1' (\acc x -> zipWith (+) acc x)
main = do
  [w,b,p] <- rl
  content <- B.getContents
  let (lw,lb) = splitAt w . map (map readNum . B.words) . B.lines $ content
  putStrLn . unlines . map show $ process w p lw lb
  where rl = fmap (map readNum . B.words) B.getLine
        readNum = fst . fromJust . B.readInt
-}
-- Does It Fit?

{-
process :: Int -> Int -> [[String]] -> [String]
process _ _ [] = []
process w'' h'' (["R",xx,yy]:shapes)
  | w''' >= x''' && h''' >= y''' = "YES" : process w'' h'' shapes
  | maxx >= x && maxy >= y = "YES" : process w'' h'' shapes
  | otherwise = "NO" : process w'' h'' shapes
  where x'' = read xx :: Int
        y'' = read yy :: Int
        x' = read xx :: Double
        y' = read yy :: Double
        w' = fromIntegral w'' :: Double
        h' = fromIntegral h'' :: Double
        maxPair (a,b) = if a > b then (a,b) else (b,a)
        (w,h) = maxPair (w',h')
        (x,y) = maxPair (x',y')
        (x''',y''') = maxPair (x'',y'')
        (w''',h''') = maxPair (w'',h'')
        a = x * (sin $ atan (h/w))
        b = h - a
        maxx = sqrt $ w^2+h^2
        maxy = (h - a) * x / a
process w h (["C",rr]:shapes)
  | w >= r && h >= r = "YES" : process w h shapes
  | otherwise = "NO" : process w h shapes
  where r = 2 * read rr :: Int

main = do
  [w,h] <- rl
  [n] <- rl
  content <- getContents
  let l = map words . lines $ content
  putStrLn . unlines $ process w h l
  where rl = fmap (map readNum . words) getLine
        readNum = read :: String -> Int

-}

{- XXX: Did'nt pass 2 cases!!

process :: Int -> Int -> [[String]] -> [String]
process _ _ [] = []
process w'' h'' (["R",xx,yy]:shapes)
  | w >= x && h >= y = "YES" : process w'' h'' shapes
  | maxx >= x && maxy >= y = "YES" : process w'' h'' shapes
  | otherwise = "NO" : process w'' h'' shapes
  where x' = read xx :: Double
        y' = read yy :: Double
        w' = fromIntegral w'' :: Double
        h' = fromIntegral h'' :: Double
        maxPair (a,b) = if a > b then (a,b) else (b,a)
        (w,h) = maxPair (w',h')
        (x,y) = maxPair (x',y')
        a = x * (sin $ atan (h/w))
        b = h - a
        maxx = sqrt $ w^2+h^2
        maxy = (h - a) * x / a
process w h (["C",rr]:shapes)
  | w >= r && h >= r = "YES" : process w h shapes
  | otherwise = "NO" : process w h shapes
  where r = 2 * read rr :: Int

main = do
  [w,h] <- rl
  [n] <- rl
  content <- getContents
  let l = map words . lines $ content
  putStrLn . unlines $ process w h l
  where rl = fmap (map readNum . words) getLine
        readNum = read :: String -> Int
-- Processing Time
import qualified Data.ByteString.Char8 as B
import Data.Maybe

bsearch :: Int -> Int -> Int -> [Int] -> Int
bsearch vl vh n l
  | vm == vl   = vh
  | go vm >= n = bsearch vl vm n l
  | go vm < n  = bsearch vm vh n l
  where go t = sum . map (t `div`) $ l
        vm = (vl + vh) `div` 2
main = do
  [n,m] <- rl
  content <- B.getContents
  let l = map readNum . B.words $ content
      (minv, maxv) = (minimum l, maximum l)
      len = length l
      t1 = n * minv `div` len
      t2 = n * maxv `div` len
      t3 = bsearch t1 (2*t2+1) n l -- cannot reason. so *2
  print t3
  where rl = fmap (map readNum . B.words) B.getLine
        readNum = (fst . fromJust . B.readInt)
-}

-- Make Our Customers Happy
import Data.List
import Data.Ord

data Order = Order { a :: Int
                   , b :: Int
                   , c :: Int
                   , ab :: Int
                   , bc :: Int
                   , ac :: Int
                   , abc :: Int
                   } deriving (Show)
oempty = Order 0 0 0 0 0 0 0
foo = Order 1 0 0 1 0 1 0

process :: Order -> [String] -> Order
process order [] = order
process order (x:xs) = process (inc x order) xs
  where inc x order = case x of
          "A" -> order { a = a order + 1 }
          "B" -> order { b = b order + 1 }
          "C" -> order { b = c order + 1 }
          "AB" -> order { ab = ab order + 1 }
          "BC" -> order { bc = bc order + 1 }
          "AC" -> order { ac = ac order + 1 }
          "ABC" -> order { abc = abc order + 1 }

min' :: Int -> Int -> Int
min' 0 y = y
min' y 0 = y
min' x y = min x y
max3, min3, min3' :: Int -> Int -> Int -> Int
min3 x y z = let x' = min x y in min x' z
min3' x y z = let x' = min' x y in min' x' z
max3 x y z = let x' = max x y in max x' z

comb :: Eq a => Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n l = [ x:y | x <- l, y <- comb (n-1) (l \\ [x]) ]

calc :: Order -> (Int, Int, Int) -> Int
calc order (na,nb,nc)
  =   let v1 = [(a order, 0, 0), (0, b order, 0), (0,0,c order)]
          v2 = [(ab order, ab order, 0),
                (ac order, 0, ac order),
                (0, bc order, bc order)]
          v2' = comb 3 v2
          v3 = (abc order, abc order, abc order)
          v = map (\x -> v1 ++ x ++ [v3]) v2'
      in fst . head . reverse . sortBy (comparing fst) $ map (foldl go (0,(na,nb,nc))) v
  where go acc'@(acc,(na,nb,nc)) (a,b,c)
          | na == 0 && a /= 0 = acc'
          | nb == 0 && b /= 0 = acc'
          | nc == 0 && c /= 0 = acc'
          | otherwise =
              let (n1, n2, n3) = (min na a, min nb b, min nc c)
                  v = min3' n1 n2 n3
                  (n1',n2',n3') = (f n1 v, f n2 v, f n3 v)
                  f x y = if x == 0 then 0 else y
              in (acc+v, (na-n1',nb-n2',nc-n3'))
--                case (n1,n2,n3) of (0,0,0) -> acc'
--                                    otherwise -> (acc+v, (na-n1',nb-n2',nc-n3'))
bar :: (Int,Int,Int) -> (Int,Int,Int) -> (Int, (Int,Int,Int))
bar (na,nb,nc) (a,b,c) = foldl go (0,(na,nb,nc)) [(a,b,c)]
  where go acc'@(acc,(na,nb,nc)) (a,b,c)
          | na == 0 && a /= 0 = acc'
          | nb == 0 && b /= 0 = acc'
          | nc == 0 && c /= 0 = acc'
          | otherwise =
              let (n1, n2, n3) = (min na a, min nb b, min nc c)
                  v = min3' n1 n2 n3
                  (n1',n2',n3') = (f n1 v, f n2 v, f n3 v)
                  f x y = if x == 0 then 0 else y
              in (acc+v, (na-n1',nb-n2',nc-n3'))
main = do
  [na,nb,nc] <- rl
  [m] <- rl
  content <- getContents
  let l = map (sort . filter (/= ',')) . lines $ content
      order = process oempty l
  print $ calc order (na,nb,nc)
  where rl = fmap (map readNum . words) getLine
        readNum = read :: String -> Int

