-- Gears of War
{-
main :: IO ()
main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if even x then "YES" else "NO") . read) . lines
-}
-- Lighthouse
{-
{-# LANGUAGE QuasiQuotes #-}
import Data.Array
import Text.RawString.QQ
type Board = Array (Int,Int) Bool -- True if vacant
type Pos   = (Int,Int) 
makeBoard :: [String] -> Board
makeBoard (hdr:board)
  = listArray ((1,1),(n,n)) $ map (\x -> if x == '*' then False else True) $ concat board
  where n = read hdr

checkBoard :: Board -> Int
checkBoard board = maximum $ map (\p -> checkPos p board) (indices board)

checkPos :: Pos -> Board -> Int
checkPos p board
  | board ! p == False = -1
  | otherwise          = checkPos' 1 p board

checkPos' :: Int -> Pos -> Board -> Int
checkPos' r (y,x) board
  | x-r < 1 ||
    x+r > n ||
    y-r < 1 ||
    y+r > n     = r-1
  | all id $ map go (assocs board) = checkPos' (r+1) (y,x) board
  | otherwise                    = r-1
  where (_,(n,_)) = bounds board
        go ((y',x'),v) = if dist (y,x) (y',x') <= f r
                         then v
                         else True
        f = fromIntegral
        dist (a,b) (c,d) = sqrt ((f a-f c)^2 + (f b-f d)^2)

main = getContents >>=
  print . checkBoard . makeBoard . lines

foo:: String
foo=[r|
**.**
*...*
.....
*...*
**.**
|]
-}
-- Treasure Hunting
{-
type Pos = (Double, Double)
rotateCC, rotateCW :: Pos -> Double -> Pos
rotateCC (x,y) theta = (x*c - y*s, x*s + y*c)
  where c = cos theta
        s = sin theta
rotateCW p theta = rotateCC p (negate theta)

dist :: Pos -> Double
dist (a,b) = sqrt (a^2 + b^2)

calc :: [Int] -> (Double, Double)
calc [x,y,a,b] = (x'/l, y'/l)
  where f = fromIntegral
        l = dist (f a,f b)
        (x',y') = rotateCW (f x, f y) $ atan (f b / f a)

main = getContents >>=
  (\(x,y) -> putStrLn $ show x ++ "\n" ++ show y) . calc . concatMap (map read. words) . lines
-}
-- Unexpected Problem
{-
import Debug.Trace
findRepetition :: String -> String -> String -> Int -> Int -> Int
findRepetition _ [] [] _ patlen -- abcabc
  = patlen
findRepetition _ [] _ idx _ -- abcabca
  = idx-1
findRepetition str remain [] idx patlen
  = findRepetition str remain (take patlen str) idx patlen
findRepetition str (x:remain) (p:pattern) idx patlen
  | x == p    = findRepetition str remain pattern (idx+1) patlen
  | otherwise = findRepetition str remain (take idx str) (idx+1) idx
findRep :: String -> Int
findRep []  = 0
findRep str = findRepetition str (tail str) (take 1 str) 2 1

calc :: String -> Int -> Int
calc str m = (m `div` r) `mod` (10^9+7)
  where r = findRep str

main = do
  s <- getLine
  m <- readLn :: IO Int
  print $ calc s m
        
foo1 = concat [ z | y <- [1..300], let z = concatMap (replicate y) ['a'..'z'] ]
foo2 = concat . replicate 20000 $ ['a'..'z']
-}
-- Enclosure
{-
import Debug.Trace
import Data.List
import Text.Printf

type Pos = (Double, Double)
type Polygon = [Int]

eps,big :: Double
eps = 0.1^8
big = 10^3
same :: Double -> Double -> Bool
same x y
  | abs(x-y) < eps = True
  | otherwise      = False
    
getRadian :: Double -> Double -> Double
getRadian r d = 2 * asin (d / (2*r))
angleSum :: Double -> Polygon -> Double
angleSum r = sum . map (getRadian r . fromIntegral)

dist :: Pos -> Pos -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
rInPoly :: Polygon -> Bool
rInPoly poly = if angleSum r (tail poly) < pi then False else True
  where r = fromIntegral (head poly) / 2 :: Double

bsearch :: Double -> Double -> (Double -> Ordering) -> Double
bsearch minv maxv checkf
--  | trace (show minv ++ "-" ++ show maxv) False = undefined
  | r == EQ = midv
  | r == LT = bsearch minv midv checkf
  | r == GT = bsearch midv maxv checkf
  where midv = (minv+maxv)/2
        r = checkf midv
calc :: Polygon -> [Pos]
calc poly
  | rInPoly poly = let (minv,maxv) = (r, f (sum poly) / 4)
                       v = bsearch minv maxv comp1
                   in draw1 v poly
  | otherwise    = let (minv,maxv) = (r, big)
                       v = bsearch minv maxv comp2
                   in draw2 v poly
  where r = f (head poly) / 2
        f = fromIntegral
        comp1 r
          | same s (2*pi) = EQ
          | s < 2*pi    = LT
          | otherwise = GT
          where s = angleSum r poly
        comp2 r
          | same s1 s2 = EQ
          | s1 > s2    = LT
          | otherwise = GT
          where s1 = angleSum r $ tail poly
                s2 = angleSum r [head poly]

draw1, draw2 :: Double -> Polygon -> [Pos]
draw1 r poly
  = map (adjCoord . radToPos r) . scanl' go initRad $ init poly
  where d = f(head poly) / 2
        f = fromIntegral
        initRad = pi + getRadian r (fromIntegral $ head poly) / 2
        go rad d = rad - theta
          where theta = getRadian r $ fromIntegral d
        radToPos r theta = (r * cos theta, r * sin theta)
        adjCoord (x,y) = (x+sqrt(r^2-(f(head poly)/2)^2), y+f(head poly)/2)
draw2 r poly
  = map (adjCoord . radToPos r) $ (negate initRad) : (scanl' go initRad $ tail (init poly))
  where d = f(head poly) / 2
        f = fromIntegral
        initRad = getRadian r (fromIntegral $ head poly) / 2
        go rad d = rad - theta
          where theta = getRadian r $ fromIntegral d
        radToPos r theta = (r * cos theta, r * sin theta)
        adjCoord (x,y) = (x-sqrt(r^2-(f(head poly)/2)^2), y+f(head poly)/2)
main = do
  n <- rl
  l <- rl
  let r = calc l
  mapM_ (\(x,y) -> putStr $ printf "%.9f\n%.9f\n\n" x y) r
  where rl = fmap (map (read :: String->Int) . words) getLine

test1 = calc [1,2,1,2]
test2 = calc [1,1,1]
test3 = calc [3,2,2] -- r=4/sqrt(7)=1.5118578920369088
-}
-- Gravity Tree
{-
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Vector.Mutable as MV
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import Debug.Trace

type Tree = Vector Node
data Node = Node { parent :: Int
                 , level :: Int
                 , children :: [Int]
                 , info :: [Int]
                 } deriving (Eq, Show)

sumList :: [Int] -> [[Int]] -> [Int]
sumList acc [] = acc
sumList [] (x:xs) = sumList x xs
sumList acc (x:xs)
  | length acc < length x = sumList (zipWith (+) (acc++repeat 0) x) xs
  | otherwise             = sumList (zipWith (+) acc (x++repeat 0)) xs
readTree :: Int -> [Int] -> Tree
readTree n parentList = runST $ do
  mvec <- GM.replicate n $ Node 0 0 [] []
  loop mvec (zip [1..n-1] parentList')
  infoUpdate mvec 0 0 
  V.freeze mvec
  where parentList' = map (\x -> x-1) parentList
        loop mv [] = return ()
        loop mv ((index,parent):xs) = do
          w <- MV.read mv index
          MV.write mv index $ w { parent = parent }
          v <- MV.read mv parent
          MV.write mv parent $ v { children = index: children v }
          loop mv xs
        infoUpdate mv level root = do
          v <- MV.read mv root
          mapM_ (infoUpdate mv (level+1)) (children v)
          l <- mapM (fmap info . MV.read mv) $ children v
          let s = sumList [] l
          MV.write mv root $ v { info = length (children v) : s
                               , level = level }
lca, lca2 :: Tree -> Int -> Int -> Int
lca2 tree u v = lastMatch ul vl 0
  where traverseUp tree x = reverse $ traverseUp' tree x
        traverseUp' tree x
          | x == 0 = [0]
          | otherwise     = x:traverseUp' tree (parent (tree ! x))
        ul = traverseUp tree u
        vl = traverseUp tree v
        lastMatch _ [] z = z
        lastMatch [] _ z = z
        lastMatch (x:xs) (y:ys) z
          | x == y    = lastMatch xs ys x
          | otherwise = z
lca tree u v
  | lu < lv = lca tree u $ parent nv
  | lu > lv = lca tree (parent nu) v
  | u == v  = u
  | otherwise = lca tree (parent nu) (parent nv)
  where nu = tree ! u
        nv = tree ! v
        lu = level nu
        lv = level nv
dist :: Tree -> Int -> Int  -> Int
dist tree node ancestor
  | node == ancestor = 0
  | otherwise        = 1 + dist tree (parent $ tree ! node) ancestor

calcValue, calcValue2 :: Tree -> Int -> Int -> Int
calcValue tree baseVal root
  = baseVal^2 + sum [ calcValue tree (baseVal+1) x | x <- children (tree ! root)]
calcValue2 tree baseVal root
  | otherwise = baseVal^2 + go (baseVal+1) (info $ tree ! root)
  where go b [] = 0
        go b (0:_) = 0
        go b (x:xs) = b^2*x + go (b+1) xs
calcValue3 :: Tree -> Int -> Int -> Int -> Int
calcValue3 tree baseVal node ancestor
  = go tree' baseVal node ancestor
  where tree' = convTree tree node ancestor
        go t b node ancestor
          | node == ancestor = calcValue2 t b node
          | otherwise        = calcValue2 t b node + go t (b+1) (parent $ t!node) ancestor
convTree :: Tree -> Int -> Int -> Tree
convTree tree node ancestor = runST $ do
  mvec <- V.thaw tree
  loop mvec node ancestor (-1) []
  where loop mv node ancestor prevNode prevInfo = do
          v <- MV.read mv node
          let info' = info v
          MV.write mv node $ v { children = delete prevNode (children v)
                               , info = diffInfo info' prevInfo }
          if node == ancestor
            then V.freeze mv
            else loop mv (parent v) ancestor node info'

diffInfo:: [Int] -> [Int] -> [Int]
diffInfo info [] = info
diffInfo x y = zipWith (-) x (1:y++repeat 0)

calc :: Tree -> [Int] -> Int
calc tree [u,v] -- v is turned on
  | w /= v = calcValue2 tree (d_uw+d_vw) v
  | w == v = calcValue3 tree 0 u v
  where w = lca tree u v
        d_uw = dist tree u w
        d_vw = dist tree v w

main = do
  [n] <- rl
  pv <- rl
  [q] <- rl
  content <- B.getContents
  let tree = readTree n pv
      l = map (map (fst . fromJust . B.readInt) . B.words) . B.lines $ content
  mapM_ (putStrLn . show . calc tree . map pred) l
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine

test1 = readTree 5 [1,2,2,4]
test2 = readTree 5 [1,2,3,4]
test3 = readTree 5 [1,1,2,4] -- (2,0)
test4 = readTree 3 [1,1]

--t = [(readTree 5 [w,x,y,z], (w,x,y,z)) | w <- [1..4], x <-[1..4], y<-[1..4], z<-[1..4]]
--tt = [ (calc x [u,v] == calc2 x [u,v], (y,u,v))| (x,y) <- t, u <- [0..4],v<-[0..4]]
foo = V.fromList [1,2,3]
bar = V.accum go foo [(1,0),(2,1)]
  where go v index = v + bar V.! index

baz = runST $ do
  mvec <- GM.replicate 3 1
  go 3 mvec
  where go 0 v = V.freeze v
        go n v = do
          MV.modify v (*2) (n-1)
          MV.modify v (*2) (n-1)
          go (n-1) v
{-# INLINE calcValue2 #-}
-}

-- Sasha and the Swaps II
{-
module Main where
import qualified Data.Set as S

swapList :: Eq a => [a] -> [[a]]
swapList [a,b] = [[b,a]]
swapList (x:xs) = (map (x:) $ swapList xs) ++ [ y:replace xs y x | y <- xs ]

swapLists :: (Ord a, Eq a) => [[a]] -> [[a]]
swapLists = S.toList . go S.empty
  where go :: (Ord a, Eq a) => S.Set [a] -> [[a]] -> S.Set [a]
        go acc []       = acc
        go acc (xs:xss) = let res = S.fromList (swapList xs)
                          in go (S.union acc res) xss

calc :: (Ord a) => [[a]] -> Int -> [[[a]]]
calc _ 0 = []
calc xss n = let r = swapLists xss
             in r:calc r (n-1)

solve :: Int -> [Int]
solve n = map length $ calc [[1..n]] (n-1)

main = do
  n <- readLn :: IO Int
  putStrLn . unwords . map show . solve $ n

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) old new
  | x == old  = new:xs
  | otherwise = x:replace xs old new
-}
{-
import Data.Array
big = 10^9+7

genArray n = res
  where res = accumArray go 0 ((1,1),(n,n)) [((i,j), (i,j)) | j <- [1..n], i<-[j..n]]
        go _ (i,j)
          | i == 1    = 0
          | j == 1    = (i * (i-1) `div` 2) `mod` big
          | i <= j     = res ! (i,i-1)
          | otherwise = ((res ! (i-1,j)) + (i-1)*(res ! (i-1,j-1))) `mod` big

calc n = [ arr ! (n,x) | x <- [1..n-1] ]
  where arr = genArray n
-}
{-
-- OEIS A087644. 
import Data.List
import qualified Data.ByteString.Char8 as B
big = 10^9+7
gen :: (Int,[Int]) -> (Int,[Int])
gen (n, xs) = (n+1, ((n+1)*n`div`2)`mod`big : go xs (tail xs))
  where go [x] [] = [(x + n*x) `mod` big]
        go (x:xs) (y:ys) = (n*x+y)`mod` big:go xs ys

calc :: Int -> [Int]
calc n = snd $ (iterate gen (2,[1])) !! (n-2)
    
main = do
  n <- readLn :: IO Int
  B.putStrLn . B.unwords . map (B.pack . show) . calc $ n
-}
{-
#include <stdio.h>
#include <stdlib.h>
typedef long int num_t;
#define BIG 1000000007
void gen(int r, int *p_idx, num_t **p_res) {
    int i, n = *p_idx;
    num_t x, y;
    if (r == 0) return;
    x = (*p_res)[n-2];
    (*p_res)[n-1] = (x+n*x) % BIG;
    for (i = n-2; i > 0; i--) {
        x = (*p_res)[i-1];
        y = (*p_res)[i];
        (*p_res)[i] = (x*n+y) % BIG;
    }
    (*p_res)[0] = ((n+1)*n/2) % BIG;
    (*p_idx)++;
}
void main() {
    int i, n, idx;
    num_t *res;
    scanf ("%d", &n);
    res = malloc(sizeof(num_t)*10000);
    res[0] = 1; idx = 2;
    for (i = 0; i < n-2; i++) gen (n-2-i,&idx, &res);
    for (i = 0; i < n-1; i++) printf("%ld ", res[i]);
    printf("\n");
}
-}
module Main where
import Data.List
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.ST
import Control.Monad
import qualified Data.ByteString.Char8 as B
big = 10^9+7
calc :: Int -> [Int]
calc n = take (n-1) . V.toList $ runST $ do
  mvec <- MV.replicate 10000 (0::Int)
  MV.write mvec 0 1
  loop (n-2) 2 mvec
    where loop 0 _ mvec = V.unsafeFreeze mvec
          loop idx n mv = do
            x <- MV.read mv (n-2)
            MV.write mv (n-1) $ (x+n*x) `mod` big
            flip mapM_ [n-2,n-3..1] $ \i -> do
              x <- MV.read mv (i-1)
              y <- MV.read mv i
              MV.write mv i $ (x*n+y) `mod` big
            MV.write mv 0 $ (((n+1)*n) `div` 2) `mod` big
            loop (idx-1) (n+1) mv
gen :: (Int,[Int]) -> (Int,[Int])
gen (n, xs) = (n+1, ((n+1)*n`div`2)`mod`big : go xs (tail xs))
  where go [x] [] = [(x + n*x) `mod` big]
        go (x:xs) (y:ys) = (n*x+y)`mod` big:go xs ys

calc2 :: Int -> [Int]
--calc2 n = snd $ (iterate gen (2,[1])) !! (n-2)
calc2 n = snd . head . drop (n-1) $ (iterate gen (2,[1]))
{-# INLINE gen #-}    
main = do
  n <- readLn :: IO Int
  B.putStrLn . B.unwords . map (B.pack . show) . calc2 $ n
