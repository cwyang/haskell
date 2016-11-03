-- Prison Transport
{-
{-# LANGUAGE QuasiQuotes #-}
import Data.Bits
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.IO
import Data.Array((!), listArray, Array)
import Data.Array.Unboxed(UArray)
import Debug.Trace
import Text.RawString.QQ

data UnionFind s = UnionFind { ids:: STUArray s Int Int
                             , szs:: STUArray s Int Int
                             }
newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftA2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftA2 (==) (root uf p) (root uf q)
root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if (id /= i)
    then do gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
    else return i
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
  i <- root uf p
  j <- root uf q
  szi <- readArray (szs uf) i
  szj <- readArray (szs uf) j
  if i == j
    then return ()
    else if (szi < szj)
         then do writeArray (ids uf) i j
                 writeArray (szs uf) j (szi + szj)
         else do writeArray (ids uf) j i
                 writeArray (szs uf) i (szj + szi)

getSize :: (UnionFind s) -> ST s [Int]
getSize uf = do
  a <- getAssocs . ids $ uf
  b <- getElems . szs $ uf
  return . map snd . filter (\((k,v),_) -> k == v) $ zip a b

readInfo :: UnionFind s -> [[Int]] -> ST s ()
readInfo uf [] = return ()
readInfo uf ([a,b]:xs) = do
  unite uf a b
  readInfo uf xs

isqrt = ceiling . sqrt . fromIntegral
main = do
  n <- readLn :: IO Int
  m <- readLn :: IO Int
  content <- map (map read.words) . lines <$> getContents
  let v = runST $ do
        uf <- newUnionFind n
        readInfo uf . map (map pred) . take m $ content
        getSize uf
--  print v
  print . sum . map isqrt $ v

foo = runST $ do
  uf <- newUnionFind 80
  readInfo uf . map (map pred) . take 30 . tail . tail . map (map read.words) . lines $ fooData
  getSize uf
fooData = [r|80
30
40 22
60 6
22 39
43 40
22 55
48 57
42 41
22 57
6 42
33 74
70 46
4 11
6 28
22 79
61 34
77 40
4 8
72 26
62 50
72 51
1 79
34 29
77 41
2 48
43 2
62 45
43 17
19 33
76 4
35 54
|]

-}
-- scturtle's solution
{-
import Control.Monad
import Data.Graph
import Data.Tree

bs n a b = if a == b then a
           else let m = (a+b) `div` 2
                in  if m*m >= n then bs n a m
                    else bs n (m+1) b
                         
main = do
  n <- readLnInt
  m <- readLnInt
  edges <- replicateM m readLnInts
  let g = buildG (1, n) . concat $ map (\[a, b] -> [(a, b), (b, a)]) edges
      sizes = map (length . flatten) . dff $ g
  print . sum . map (\ n -> bs n 1 320) $ sizes
        
  where readLnInt = readLn :: IO Int
        readLnInts = liftM (map (read :: String -> Int) . words) getLine
-}                                                                                                                                                                                                                                      
-- jirkamarsik's solution
import Control.Applicative
import Control.Monad
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Tree

solve :: Gr () () -> Int
solve g = sum $ map busCostForGroup $ components g
  where busCostForGroup group = ceiling $ sqrt $ fromIntegral $ length group

main :: IO ()
main = do n <- readLn
          m <- readLn
          edges <- replicateM m $ do [from, to] <- map read <$> words <$> getLine
          return (from, to)
          print $ solve $ mkUGraph [1..n] edges
                                                                                            
