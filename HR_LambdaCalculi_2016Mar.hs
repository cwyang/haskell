-- 20160325 Lambda Calculi 
-- 1. EASY Function or Not
{-
import Data.List
import Data.Function

makeList :: [String] -> [[(Int,Int)]]
makeList [] = []
makeList (h:t) =
  let n = read h
      (l, rest) = splitAt n t
  in map ((\[x, y] -> (x,y)) . map (read :: String->Int) . words) l : makeList rest

foo = makeList ["3", "1 2", "3 4", "5 6", "1", "7 8"]     

isFunction:: [(Int,Int)] -> String
isFunction = (\x -> if x then "YES" else "NO") . all (\x -> length x == 1) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

main = do
    nr_test <- readLn :: IO Int
    input <- getContents
    (mapM_ (putStrLn . isFunction)) . makeList . lines $ input

-- 2. Compute the perimeter of Polygon
import Text.Printf

makeList :: [String] -> [(Double,Double)]
makeList =  map ((\[x, y] -> (x,y)) . map (read :: String->Double) . words) 
-- foo = makeList ["1 2", "3 4", "5 6"]     

computePerimeter :: [(Double,Double)] -> Double
computePerimeter l = sum $ map dist m
  where m = zip l (last l:init l)
        dist ((a,b),(c,d)) = sqrt((a - c)^2 + (b - d)^2)

main = do
    n <- readLn :: IO Int
    input <- getContents
    printf "%.1f" . computePerimeter . makeList . lines $ input

-- 3. Compute the area of Polygon (shoelace formula)
import Text.Printf
makeList :: [String] -> ([Double],[Double])
makeList =  unzip . map ((\[x, y] -> (x,y)) . map (read :: String->Double) . words) 
foo = makeList ["1 2", "3 4", "5 6"]     

computeArea :: ([Double],[Double]) -> Double
computeArea (xs,ys) = 0.5 * (a - b)
  where a = sum $ zipWith (*) xs (tail ys ++ [head ys])
        b = sum $ zipWith (*) (tail xs ++ [head xs]) ys

main = do
    n <- readLn :: IO Int
    input <- getContents
    printf "%.1f" . computeArea . makeList . lines $ input
-}
-- 4. Decide wheter a polygon is concave (gift wrapping)
-- NOT SOLVED
{-
import Text.Printf
import Data.Function
import Data.List

makeList :: [String] -> [(Double,Double)]
makeList =  map ((\[x, y] -> (x,y)) . map (read :: String->Double) . words) 

area :: ([Double],[Double]) -> Double
area (xs,ys) = abs (0.5 * (a - b))
  where a = sum $ zipWith (*) xs (tail ys ++ [head ys])
        b = sum $ zipWith (*) (tail xs ++ [head xs]) ys

gen :: [a] -> [[a]]
gen [] = []
gen (h:t) = t:map (h:) (gen t)

isConcave :: [(Double,Double)] -> Bool
isConcave x | length x == 3 = False
isConcave x = a <= as
  where x' = sortPoints x
        a = area (unzip x')
        as = maximum (map (area . unzip) (gen x'))

sortPoints :: [(Double,Double)] -> [(Double,Double)]
sortPoints ((px,py):ps) =  map snd (sortBy (compare `on` fst) l)
  where l = (0,(px,py)): map (\(x,y) -> (atan2 (y - py) (x - px), (x,y))) ps

sortPoints1 :: [(Double,Double)] -> [(Double,(Double,Double))]
sortPoints1 ((px,py):ps) =  map id (sortBy (compare `on` fst) l)
  where l = (0,(px,py)): map (\(x,y) -> (atan2 (y - py) (x - px), (x,y))) ps

isConcave :: [(Double,Double)] -> Bool
isConcave x | length x == 3 = False
isConcave x = length x < length g
  where g = giftWrap x

giftWrap :: [(Double,Double)] -> [(Double, Double)]
giftWrap ((px,py):ps) = (px,py): giftWrap' (px,py) (map snd (sortBy (compare `on` fst) l) ++ [(px,py)])
  where l = map (\(x,y) -> (atan2 (y - py) (x - px), (x,y))) ps

giftWrap' :: (Double,Double) -> [(Double,Double)] -> [(Double, Double)]
giftWrap' _ [] = []
giftWrap' i@(ix,iy) (p@(px,py):ps)
  | i == p = []
  | otherwise = (px,py): giftWrap' i (map snd (sortBy (compare `on` fst) l) ++ [(px,py)])
  where l = map (\(x,y) -> (atan2 (y - py) (x - px), (x,y))) ps
bar :: [(Double,Double)]
bar =[
  (1028, 625),
  (1042, 943),
  (793, 1042),
  (404, 909),
  (574, 474),
  (1077, 721),
  (392, 543),
  (572, 1005),
  (963, 1020),
  (857, 390)]
baz :: [(Double,Double)]
baz = [(0,0), (0,1), (1,1), (1,0)]

main = do
    n <- readLn :: IO Int
    input <- getContents
    putStrLn . (\x -> if x then "YES" else "NO") . isConcave . makeList . lines $ input
-}
-- 5. Tree Manager
{-
data Tree = NIL | Tree { parent :: Tree
                       , node :: Int
                       , left :: [Tree] -- reversed
                       , right :: [Tree]
                       , children ::[Tree]
                       } deriving (Show)

root = Tree { parent = NIL, node = 0, left = [], right = [], children = [] }

changeValue :: Tree -> Int -> Tree
changeValue t n = t { node = n }

printTree :: Tree -> String
printTree t = show . node $ t

visitLeft :: Tree -> Tree
visitLeft t = Tree { parent = parent t
                   , node = node . head . left $ t
                   , left = tail . left $ t
                   , right = t : right t
                   , children = children . head . left $ t}

visitRight :: Tree -> Tree
visitRight t = Tree { parent = parent t
                   , node = node . head . right $ t
                   , left = t : left t
                   , right = tail . right $ t
                   , children = children . head . right $ t}

visitParent :: Tree -> Tree
visitParent t = (parent t) { children = reverse (left t) ++  (t:(right t)) }

split :: [a] -> Int -> [a] -> ([a], a, [a])
split (h:t) 0 acc = (acc, h, t)
split (h:t) n acc = split t (n-1) (h:acc)

split [] n acc = ([], head acc, [])

visitChild :: Tree -> Int -> Tree
visitChild t n = child { parent = t
                       , left = l
                       , right = r}
  where (l, child, r) = split (children t) (n-1) []

insertLeft :: Tree -> Int -> Tree
insertLeft t n = t { left = newt : left t }
  where newt = Tree { parent = parent t
                    , node = n
                    , left = []
                    , right = []
                    , children = []}

insertRight :: Tree -> Int -> Tree
insertRight t n = t { right = newt : right t }
  where newt = Tree { parent = parent t
                    , node = n
                    , left = []
                    , right = []
                    , children = []}

insertChild :: Tree -> Int -> Tree
insertChild t n = t { children = newt : children t }
  where newt = Tree { parent = t
                    , node = n
                    , left = []
                    , right = []
                    , children = []}

delete :: Tree -> Tree
delete t = (parent t) { children = reverse (left t) ++ (right t) }

process :: [String] -> [String]
process = reverse . snd . foldl go (root,[])
  where go (t,log) cmd
          = t `seq` case (words cmd) of
          ["change",n] -> (changeValue t (read n), log)
          ["print"] -> (t, printTree t : log)
          ["visit", "left"] -> (visitLeft t, log)
          ["visit", "right"] -> (visitRight t, log)
          ["visit", "parent"] -> (visitParent t, log)
          ["visit", "child", n] -> (visitChild t (read n), log)
          ["insert", "right", n] -> (insertRight t (read n), log)
          ["insert", "left", n] -> (insertLeft t (read n), log)
          ["insert", "child", n] -> (insertChild t (read n), log)
          ["delete"] -> (delete t, log)

main = do
  n <- readLn :: IO Int
  input <- getContents
  putStrLn . unlines . process . lines $ input

-- 6. Heap
-}
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M

import Data.Maybe
import Data.List

newtype MultiSet = MultiSet (M.IntMap Int)
  deriving (Show)

emptyMS :: MultiSet
emptyMS = MultiSet (M.empty)

insertMS :: MultiSet -> Int -> MultiSet
insertMS (MultiSet m) n =
  MultiSet $ M.insertWith (+) n 1 m
{-# INLINE insertMS #-}

findMaxMS :: MultiSet -> (Int, Int)
findMaxMS (MultiSet m) =  M.findMax m

deleteMaxMS :: MultiSet -> MultiSet
deleteMaxMS ms@(MultiSet m) =
  let (n,v) = M.findMax m
  in case v of
    1 -> MultiSet $ M.deleteMax m
    otherwise -> MultiSet $ M.insert n (v-1) m
{-# INLINE deleteMaxMS #-}

combineMS :: MultiSet -> MultiSet -> MultiSet
combineMS (MultiSet a) (MultiSet b) =
  MultiSet $ M.union a b

process :: [B.ByteString] -> [B.ByteString]
process = map (B.pack . show) . reverse . snd . foldl' go (M.empty,[])

go :: (M.IntMap (MultiSet), [Int]) -> B.ByteString -> (M.IntMap (MultiSet), [Int])
go (m, res) str =
  m `seq`
  case cmd of
    1 -> (m, fst (findMaxMS h) : res)       -- find strongest
    2 -> (M.insert armyNo (deleteMaxMS h) m, res) -- strongest died
    3 -> (M.insert armyNo (insertMS h auxArg) m, res) -- recruit
    4 -> (M.insert armyNo (combineMS h h') (M.delete auxArg m), res) -- merge
  where (cmd:args) = map (fst . fromJust . B.readInt) . B.words $ str
        armyNo = head args
        auxArg = head . tail $ args
        h = case M.lookup armyNo m of
          Nothing -> emptyMS
          Just x  -> x
        h' = case M.lookup auxArg m of
          Nothing -> emptyMS
          Just x  -> x
main = do
  l1 <- B.getLine
  let [n,q] = map (fromJust . B.readInt) . B.words $ l1
  input <- B.getContents
  B.putStrLn . B.unlines . process . B.lines $ input

