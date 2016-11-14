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
{-
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
                                                                                            
-}
--Range Minimum Query
{-
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Debug.Trace
data SegmentTree s a = SegmentTree { size :: Int
                                   , op :: a->a->a
                                   , seg  :: STArray s Int (Maybe a)
                                   }
left, right:: Int -> Int
left x = x*2
right x = x*2+1
half :: Int -> Int -> Int
half a b = (a+b) `div` 2
newSegmentTree :: Int -> [a] -> (a->a->a) -> ST s (SegmentTree s a)
newSegmentTree n xs op = do
  SegmentTree n' op <$> newListArray (0,2*n'-1) (replicate n' Nothing ++ map Just xs ++ repeat Nothing)
  where n' = 2 ^ (ceiling (logBase 2 (fromIntegral n)))
buildSegmentTree :: (Show a, Ord a) => Int -> [a] -> (a->a->a) -> ST s (SegmentTree s a)
buildSegmentTree n xs func = do
  st <- newSegmentTree n xs func
  build' st 1 0 (size st)
  return st
  where build' :: (Ord a) => SegmentTree s a -> Int -> Int -> Int -> ST s ()
        build' st p l r
          | l == r-1 = return ()
          | otherwise = do
              build' st (left p)  l (half l r)
              build' st (right p) (half l r) r
              v1 <- readArray (seg st) (left p)
              v2 <- readArray (seg st) (right p)
              let nv = case (v1,v2) of
                    (Nothing,_) -> v2
                    (_,Nothing) -> v1
                    otherwise   -> liftA2 (op st) v1 v2
              writeArray (seg st) p nv
--querySegmentTree :: (SegmentTree s a) -> Int -> Int -> ST s (Maybe a)
querySegmentTree st i j = go st 1 0 (size st) i j
  where go :: (Ord a) => SegmentTree s a -> Int -> Int -> Int -> Int -> Int -> ST s (Maybe a)
        go st p l r i j
--          | traceShow (p,l,r,i,j) False = undefined
          | i >= r || j <= l = return Nothing
          | l == i && r == j = readArray (seg st) p
          | m < i            = go st (right p) m r i j
          | j <= m           = go st (left p)  l m i j
          | otherwise = do
              v1 <- go st (left p) l m i m
              v2 <- go st (right p) m r m j
              case (v1,v2) of
                (Nothing,_) -> return v2
                (_,Nothing) -> return v1
                otherwise   -> return $ liftA2 (op st) v1 v2
                where m = half l r
main :: IO ()
main = do
  [n,m] <- rl
  a <- rl
  queries <- replicateM m $ do
    [l,r] <- rl
    return (l,r+1)
  let res = fromJust . sequence $ runST $ do
        st <- buildSegmentTree n a min
        a <- getAssocs (seg st)
--        traceShowM (a, size st)
--        a <- (freeze $ seg st) :: ST s (Array Int (Maybe Int))
        mapM (\(x,y) -> querySegmentTree st x y) queries
  putStrLn . init . unlines . map show $ res 
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
-}
-- John and Fence
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
solve :: [Int] -> Int
solve = maximum . map solve' . tails
solve' :: [Int] -> Int
solve' = fst . foldl' go (0, (0,maxBound))
  where go (acc, (n, minv)) x = (max acc' acc, (n', minv'))
          where n'    = n + 1
                minv' = min minv x
                acc'  = n' * minv' 
  
main :: IO ()
main = do
  [n] <- rl
  a <- rl
  print $ solve a
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
