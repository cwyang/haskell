{-
-- Divisible Sum Pairs
import Data.List

countPairs :: Int -> [Int] -> Int
countPairs k l = length [(x,y) | x:xs <- tails l, y <- xs, (x+y) `rem` k == 0]

main = do
  [n,k] <- rl
  l <- rl
  print $ countPairs k l
  where rl = fmap (map (read :: String->Int) . words) getLine

-- Non divisible Subset
import Data.List
import Data.Array

countSetSize :: Int -> [Int] -> Int
countSetSize k l
  | odd k  = v0 + sumMap (take k2 zr)
  | even k = v0 + vc + sumMap (take (k2-1) zr)
  where r@(rh:rs) = elems $ accumArray (+) 0 (0,k-1) [(x `rem` k,1) | x <- l]
        zr = zip rs (reverse rs)
        k2 = k `div` 2
        v0 = if rh > 0 then 1 else 0
        vc = if r !! k2 > 0 then 1 else 0
        sumMap = sum . map (\(x,y) -> max x y)
        
main = do
  [n,k] <- rl
  l <- rl
  print $ countSetSize k l
  where rl = fmap (map (read :: String->Int) . words) getLine

-- Jogging Cats
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Maybe
import Data.List

-- num_node -> adj -> adj map
makeAdj :: Int -> [(Int,Int)] -> Array Int (M.Map Int Int)
makeAdj n adjlist = accumArray add M.empty (1,n) ([x | x<-adjlist]++[swap x | x<-adjlist])
  where swap (x,y) = (y,x)
        add m x = M.insert x x m
-- visitetd -> length -> adj map -> start -> paths
travAdj :: [Int] -> Int -> Array Int (M.Map Int Int) -> Int -> [[Int]]
travAdj _ 0 _ start = [[start]]
travAdj visited len arr start = let m = arr ! start
                                in map (start:)
                                   (concat $ map (travAdj (start:visited) (len-1) arr)
                                    [x | x <- M.elems m, x `notElem` visited])
travAdj' :: Int -> Array Int (M.Map Int Int) -> Int -> [[Int]]
travAdj' len arr start = let res = travAdj [] len arr start
                         in filter (\x -> M.member (head x) (arr ! last x)) res
getSum :: Int -> M.Map Int Int -> Array Int (M.Map Int Int) -> Int -> Int
getSum len visited arr current
  | current > maxb = 0
  | otherwise = f res' + nextSum
  where visited' = foldl' (\acc x -> M.insert x x acc) visited (concat res')
        nextSum = getSum len visited' arr (current+1)
        res = travAdj' len arr current
        res' = filter (\x -> not $ any (\y -> y < current && y `M.member` visited) x) res
        f x = (length x `div` 2)
        (minb,maxb) = bounds arr
main = do
  l1 <- B.getLine
  let [n,m] = map (fst . fromJust . B.readInt) . B.words $ l1
  content <- B.getContents
  let adj = makeAdj n $ map ((\[x,y]->(x,y)) . map (fst . fromJust . B.readInt) . B.words) . B.lines $ content
  print $ getSum 3 M.empty adj 1

foo = makeAdj 4 [(1,2),(2,3),(3,4),(4,1),(1,3),(2,4)]
foo1 = makeAdj 4 [(1,2),(2,3),(3,4),(4,1),(1,3)]
foo2 = makeAdj 6 [(1,2),(2,3),(3,4),(4,1),(2,5),(5,6),(6,3)]
bar = makeAdj 5 [(2,3),(3,4),(4,5),(5,2),(2,4),(3,5)]
-}
-- Cat Cation Rentals
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Data.Maybe
import Text.Printf

-- go :: Acc -> D -> Req -> Query -> N
go :: M.IntMap (Int,Int) -> Int -> [(Int,Int)] -> Int -> Int
go _ _ [] _ = 0
go acc d ((l,r):rs) q
  | len < q = go acc d rs q
  | overlap acc (l,r) = go acc d rs q
  | otherwise = len + go acc' d rs q
  where len = r - l + 1
        insert (x,y) m = M.insert y (x,y) $ M.insert x (x,y) m
        acc' = insert (l,r) acc

overlap :: M.IntMap (Int,Int) -> (Int,Int) -> Bool
overlap m (l,r) =
  case M.lookupGE l m of
    Nothing -> False
    Just (_,v) -> overlap' v (l,r)
  where
    overlap' :: (Int,Int) -> (Int,Int) -> Bool
    overlap' (a,b) (c,d)
          | a > d = False
          | b < c = False
          | otherwise = True
main = do
  l1 <- B.getLine
  content <- B.getContents
  let [n,d,k] = map (fst . fromJust . B.readInt) . B.words $ l1
      (r,q) = splitAt n . B.lines $ content
      req = map ((\[x,y]->(x,y)) . map (fst . fromJust . B.readInt) . B.words) $ r
      query = map (fst . fromJust . B.readInt) q
  mapM_ (B.putStrLn . B.pack . show . go M.empty d req) query

dataSize = 4000
dataGen :: IO ()
dataGen = do
  putStrLn  $ (\x -> printf "%d %d %d" x x x) dataSize
  mapM_ (putStrLn . (\x -> printf "%d %d" x x)) ([1..dataSize] :: [Int])
  mapM_ (putStrLn . (\x -> printf "%d" x)) ([1..dataSize] :: [Int])
    
