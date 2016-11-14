{-
-- BFS: shortest reach
import Data.Tree
import Data.Graph
import Data.Array
import qualified Data.IntSet as IS
import qualified Data.Sequence as SQ

bfs :: Graph -> Vertex -> [[Vertex]]
bfs graph start = go IS.empty graph $ SQ.singleton [start]
  where go :: IS.IntSet -> Graph -> SQ.Seq [Vertex] -> [[Vertex]]
        go seen graph queue =
          case SQ.viewl queue of
            SQ.EmptyL -> []
            vs SQ.:< rest -> vs : go seen'' graph queue'
              where seen' = foldr IS.insert seen vs
                    (neighbors,seen'') = foldr gogo ([],seen') vs
                    queue' | null neighbors = rest 
                           | otherwise      = rest SQ.>< SQ.singleton neighbors
                    gogo :: Vertex -> ([Vertex], IS.IntSet) -> ([Vertex], IS.IntSet)
                    gogo v (n,s) = let ns = filter (not . flip IS.member s) $ graph ! v
                                       s' = foldr IS.insert s ns
                                   in (n ++ ns, s')
shortestReach :: Graph -> Vertex -> [Int]
shortestReach graph start = elems arr
  where res = bfs graph start
        arr = accumArray (flip const) (-1) (bounds graph)
              . concat $ zipWith (\xs n -> zip xs $ repeat n) res [0,6..] 
                                      
readGraphs :: [String] -> [(Graph, Vertex)]
readGraphs [] = []
readGraphs (h:t) = (buildG (1,n) edge', read s) : readGraphs remain
  where [n,m] = map read . words $ h
        (edge,s:remain) = splitAt m t
        edge' = concatMap ((\[x,y] -> [(x,y),(y,x)]) . map read . words)  edge
main = do
    nrTest <- readLn :: IO Int
    inputdata <- getContents
    mapM_ (putStrLn . unwords . map (show :: Int->String) . uncurry shortestReach)
      . readGraphs . lines $ inputdata

g1 :: [Edge]
g1 = [
  (0,1),
  (1,2),
  (1,3),
  (3,4)]
undirFilter :: [Edge] -> [Edge]
undirFilter = concatMap (\(x,y) -> [(x,y), (y,x)])
test = shortestReach (buildG (0,4) $ undirFilter g1) 0
test2 = bfs (buildG (0,4) $ undirFilter g1) 0
test3 = concat $ zipWith (\xs n -> zip xs $ repeat n) test2 [0,6..] 
-}
--
{-- jooyunghan's code
import Control.Applicative
import Control.Monad
import Data.List (lookup)
import Data.Array
import qualified Data.Map.Strict as Map

bfs :: Int -> [(Int,Int)] -> Int -> Map.Map Int Int
bfs n edges start = iter Map.empty [(start, 0)]
  where iter :: Map.Map Int Int -> [(Int,Int)] -> Map.Map Int Int
        iter seen [] = seen
        iter seen ((i,d):xs) = case Map.lookup i seen of
          Just d' | d' > d -> iter (Map.insert i d seen) xs
          Just _ -> iter seen xs
          Nothing -> iter (Map.insert i d seen) (xs ++ [(n',d+6)| n' <- neighbors ! i])
        neighbors = accumArray (flip (:)) [] (1,n) $ edges ++ map (\(a,b) -> (b,a)) edges

format :: Int -> Map.Map Int Int -> String
format n dist = unwords [show d| i <- [1..n], let d = Map.findWithDefault (-1) i dist, d /= 0 ]

solve :: IO()
solve = do
  [n,m] <- map read . words <$> getLine
  edges <- replicateM m $ do
    [from,to] <- map read . words <$> getLine
    return (from,to)
  start <- read <$> getLine
  putStrLn $ format n $ bfs n edges start

main :: IO()
main = do
  t <- read <$> getLine
  replicateM_ t solve
--}
-- Dijkstra: Shortest Reach 2
{-
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Control.Monad
-- | Weighted Graph
type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b, (a,w))]) edges

dijkstra :: Graph Int -> S.Set (Int,Vertex) -> M.Map Vertex Int -> M.Map Vertex Int
dijkstra graph pq dist
  | S.null pq                  = dist
  | d_u' /= Nothing && d_u < d = dijkstra graph pq' dist
  | otherwise                  = dijkstra graph new_pq new_dist
  where ((d,u), pq') = S.deleteFindMin pq
        d_u' = M.lookup u dist
        d_u = case d_u' of {Just v -> v; Nothing -> 0}
        (new_pq, new_dist) = foldr go (pq', dist) [ (x,w) | (x,w) <- graph ! u ]
        go (x,w) (pq,dist)
          | d_x' == Nothing         = res
          | d_u + w < fromJust d_x' = res
          | otherwise               = (pq, dist)
          where d_x' = M.lookup x dist
                new_d = d_u + w
                res = (S.insert (new_d, x) pq, M.insert x new_d dist)

dijkstra' :: Graph Int -> Vertex -> M.Map Vertex Int
dijkstra' graph start = dijkstra graph (S.singleton (0,start)) (M.singleton start 0)

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  t <- readInt <$> B.getLine
  replicateM_ t solve
  
solve :: IO()
solve = do
  [n,m] <- map readInt . B.words <$> B.getLine
  edges <- replicateM m $ do
    [from,to,w] <- map readInt . B.words <$> B.getLine
    return (from,to,w)
  start <- readInt <$> B.getLine
  let graph = graphFromEdges (1,n) edges
      sssp = dijkstra' graph start
  putStrLn $ format n sssp
  where format :: Int -> M.Map Int Int -> String
        format n dist = unwords [show d| i <- [1..n], let d = M.findWithDefault (-1) i dist, d /= 0 ]

g1 :: [Edge Int]
g1 = [
  (0,2,6),
  (0,4,1),
  (1,2,2),
  (1,3,3),
  (1,4,6),
  (2,3,7),
  (3,4,5)]
test = graphFromEdges (0,4) g1
test2 = dijkstra' test 2
-}
---
{- other guy of hackerrank
import Data.Functor
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main = do
  t <- read <$> getLine
  replicateM_ t oneCase

oneCase = do
  [n, m] <- map read <$> (words <$> getLine)
  edgesInput <- replicateM m $ do
    [x, y, r] <- map read <$> (words <$> getLine)
    return (x, y, r)
  s <- read <$> getLine

  let
    edges = eat (M.fromList (zip [1..n] (repeat []))) edgesInput where
      eat edges ((x,y,r) : es) = eat edges' es where
        edges' =
          M.adjust ((x, r) :) y .
          M.adjust ((y, r) :) x $
          edges
      eat edges _ = edges

    dijkstra
      :: M.Map Int Int -- runing node with distance
      -> S.Set (Int, Int) -- runing heap (distance, node)
      -> [(Int, Int)] -- done node with distance
    dijkstra nodeDis heap
      | S.null heap = M.toAscList nodeDis
      | otherwise =
        let
          ((dis, node), heap') = S.deleteFindMin heap
          (nodeDis'', heap'') = relax nodeDis heap' (edges M.! node) where
            relax nodeDis heap ((u, r) : es) =
              let
                uDis = nodeDis M.! u
                uDis' = if dis < maxBound then dis + r else maxBound

                heap' = S.insert (uDis', u) . S.delete (uDis, u) $ heap
                nodeDis' = M.insert u uDis' nodeDis
              in
                if uDis' < uDis then
                  relax nodeDis' heap' es
                else
                  relax nodeDis heap es
            relax nodeDis heap _ = (nodeDis, heap)
        in
          dijkstra nodeDis'' heap''

    res = dijkstra
      (M.fromList (map (\v -> if v == s then (v, 0) else (v, maxBound)) [1..n]))
      (S.fromList (map (\v -> if v == s then (0, v) else (maxBound, v)) [1..n]))

  putStrLn $ unwords . map (show . (\x -> if x == maxBound then (-1) else x) . snd) . filter ((/= s). fst) $ res
-}
-- Prim's (MST) : Special Subtree
{-
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Control.Monad
-- | Weighted Graph
type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b, (a,w))]) edges

prim :: Graph Int -> Vertex -> Int
prim graph start = prim' pq taken 0
  where (pq, taken) = process start S.empty S.empty
        process vertex pq taken = (pq', taken')
          where taken' = S.insert vertex taken
                pq'    = foldr S.insert pq [ (w, x) | (x,w) <- graph ! vertex,
                                                        not (S.member x taken) ]
                -- sort by (inc) weight then by (inc) id
        prim' pq taken mst_cost
          | S.null pq         = mst_cost
          | S.member id taken = prim' pq' taken mst_cost
          | otherwise         = let (pq'', taken') = process id pq taken
                                in traceShow id $ prim' pq'' taken' (mst_cost + weight)
          where ((weight, id), pq') = S.deleteFindMin pq

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,m] <- map readInt . B.words <$> B.getLine
  edges <- replicateM m $ do
    [from,to,w] <- map readInt . B.words <$> B.getLine
    return (from,to,w)
  start <- readInt <$> B.getLine
  let graph = graphFromEdges (1,n) edges
      mst = prim graph start
  print mst

mdef v = (fromMaybe v .) . M.lookup
-}
-- Kruskal (MST): Really Special Subtree
{-
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Array.ST
import Data.Maybe
import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Data.Functor
import Control.Applicative
import Data.List (sort)
-- | Weighted Graph
type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
toEdgeList :: [(Vertex,Vertex,a)] -> [(a, (Vertex,Vertex))]
toEdgeList = map (\(a,b,w) -> (w, (a,b)))
type EdgeList a = [(a, (Vertex, Vertex))]

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b, (a,w))]) edges

kruskal :: (Int,Int) -> EdgeList Int -> Int
kruskal (s,e) edges = runST $ do
  uf <- newUnionFind (s,e) (e-s+1)
  kruskal' (sort edges) 0 uf
  where kruskal' :: EdgeList Int -> Int -> UnionFind s -> ST s Int
        kruskal' [] cost _ = return cost
        kruskal' ((w,(u,v)):es) cost uf = do
          cycle <- find uf u v
          if cycle then
            kruskal' es cost uf
           else do
--            traceM (show (u,v,w))
            unite uf u v
            kruskal' es (cost+w) uf

data UnionFind s = UnionFind { ids:: STUArray s Int Int, szs:: STUArray s Int Int}
newUnionFind :: (Int,Int) -> Int -> ST s (UnionFind s)
newUnionFind bound n = liftA2 UnionFind (newListArray bound $ range bound) (newArray bound 1)
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftA2 (==) (root uf p) (root uf q)
root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if (id /= i)
    then do {gpid <- readArray (ids uf) id; writeArray (ids uf) i gpid; root uf id}
    else return i
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do { i <- root uf p; j <- root uf q;
                    szi <- readArray (szs uf) i; szj <- readArray (szs uf) j;
                    if (szi < szj)
                    then do {writeArray (ids uf) i j; writeArray (szs uf) j (szi + szj)}
                    else do {writeArray (ids uf) j i; writeArray (szs uf) i (szj + szi)}}

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,m] <- map readInt . B.words <$> B.getLine
  edges <- replicateM m $ do
    [from,to,w] <- map readInt . B.words <$> B.getLine
    return (from,to,w)
  start <- readInt <$> B.getLine
  let edgeList = toEdgeList edges
      cost = kruskal (1,n) edgeList
  print cost
-}
--
{-- quick_dudley@hackerrank 

import Data.List (sortBy, words)
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST

newtype UM a = UM {runUM' :: forall s. (STArray s Int (Maybe Int)) -> ST s a}

instance Functor UM where
  fmap f (UM a) = UM ((fmap . fmap) f a)

instance Applicative UM where
  (<*>) = ap
  pure = return

instance Monad UM where
  return a = UM (\g -> return a)
  (UM a) >>= f = UM (\g -> (fmap (runUM' . f) (a g)) >>= ($ g))

runUM :: Int -> UM a -> a
runUM n (UM u) = runST (newArray (1,n) Nothing >>= u)

umGet :: Int -> UM Int
umGet i = UM $ \g -> fmap (\x -> case x of {Nothing -> i; Just v -> v}) $ readArray g i
umPut :: Int -> Int -> UM ()
umPut i v = UM $ \g -> writeArray g i (Just v)

uFind :: Int -> UM Int
uFind i = do
  j <- umGet i
  if i == j
    then return j
    else do
      k <- uFind j
      if k == j then return () else umPut i k
      return k

uMerge :: Int -> Int -> UM Bool
uMerge a b = do
  a' <- uFind a
  b' <- uFind b
  if a' == b'
    then return False
    else umPut a' b' >> return True

kruskal :: Int -> [(Int,Int,Integer)] -> Integer
kruskal n e = sum $
  runUM n $ forM (sortBy (\ ~(_,_,a) ~(_,_,b) -> a `compare` b) e) $
    \ ~(a,b,w) -> fmap (\v -> if v then w else 0) $ uMerge a b

main = do
  ~(n:m:_) <- fmap (map read . words) getLine
  e <- forM [1 .. m] $ const $
    fmap ((\ ~(a:b:w:_) -> (fromIntegral a, fromIntegral b,w)) . map read . words) getLine
  s <- fmap read getLine :: IO Int -- unused
  print $ kruskal n e
-}
{-- odomontois@hackerrank

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module Main where

import           Control.Monad.State.Strict
import           Data.IntMap                (IntMap, (!))
import qualified Data.IntMap                as IntMap
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing)
import           Control.Applicative

type DisjointSets = IntMap (Either Int Int)

type DM m = MonadState DisjointSets m

initial::Int->DisjointSets
initial n = IntMap.fromAscList $ fmap (, Right 1) [1..n]

group::DM m=>Int->m (Int, Int)
group num = do
  qual <- gets (! num)
  case qual of
    Right size -> return (num, size)
    Left parent -> do
      (g,size) <- group parent
      if g == parent then return (g, size) else do
        modify . IntMap.insert num $ Left g
        return (g, size)

assign::DM m=>Int->Int->m()
assign i j = do
  (gi,si) <- group i
  (gj,sj) <- group j
  unless (gi == gj) $ do
    let (small, large) = if si < sj then (gi, gj) else (gj, gi)
    modify . IntMap.insert small $ Left large
    modify . IntMap.insert large . Right $ si + sj

consider::DM m=>Int->Int->m Bool
consider i j = do
  (gi, _) <- group i
  (gj, _) <- group j
  if gi == gj then return False else do
    assign i j
    return True

main :: IO ()
main = do
  let getNums = fmap (fmap read.words) getLine
  let edge [i,j,s] = ((i,j),s)
      edge _       = undefined
  [n,m] <- getNums
  edges <- fmap edge <$> replicateM m getNums
  void getLine
  let frame = filterM (uncurry consider.fst) $ sortBy (comparing snd) edges
  let frameCost = sum. fmap snd . evalState frame $ initial n
  print frameCost
--}
{-- fabian99m@hackerank

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List

main = do
    [n,m] <- fmap (map read . words) getLine
    interact $ show . kruskal . map parse . take m . lines
    
parse :: String -> Edge
parse s = EdgeT u w v
    where
        [u,v,w] = map read . words $ s

kruskal :: [Edge] -> Int
kruskal [] = 0
kruskal ls = go 0 sorted Map.empty
    where
        sorted = sort ls
        go acc [] _ = acc
        go acc (EdgeT u w v:xs) m =
            if connected u v m
                then go acc xs m
                else go (acc+w) xs (connect u v m)
                
lookupDef k m = Map.findWithDefault (Set.singleton k) k m
                
connected u v m = Set.member u $ lookupDef v m

connect u v m = Set.foldl' (\m' x -> Map.insert x resSet m') m resSet
    where
        set k = lookupDef k m
        resSet = set u `Set.union` set v
                                   

data Edge = EdgeT Int Int Int
    deriving Show

instance Eq Edge where
    (==) (EdgeT u1 w1 v1) (EdgeT u2 w2 v2) = w1==w2 && u1 + w2 + v1 == u2 + w2 + v2

instance Ord Edge where
    compare (EdgeT u1 w1 v1) (EdgeT u2 w2 v2)
        | w1<w2 = LT
        | w1>w2 = GT
        | u1 + w1 + v1 < u2 + w2 + v2 = LT
        | u1 + w1 + v1 > u2 + w2 + v2 = GT
        | otherwise = EQ
-}
-- Even Tree
{-
import Data.Functor
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Maybe
import Data.Graph
import Data.Tree
import Debug.Trace
import Control.Monad
import Control.Applicative
import Data.Monoid

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

undirFilter :: [Edge] -> [Edge]
undirFilter = concatMap (\(x,y) -> [(x,y), (y,x)])

mapTree :: (Tree a -> b) -> Tree a -> Tree b
mapTree f t@(Node x ts) = Node (f t) (map (mapTree f) ts)

countNode :: Tree a -> Tree Int
countNode (Node x []) = (Node 1 [])
countNode (Node x ts) = let ts' = map countNode ts
                            s   = sum $ map rootLabel ts'
                        in (Node (s+1) ts')
                           
main :: IO ()
main = do
  [n,m] <- map readInt . B.words <$> B.getLine
  edges <- replicateM m $ do
    [from,to] <- map readInt . B.words <$> B.getLine
    return (from,to)
  let graph = buildG (1,n) $ undirFilter g1
      tree  = head $ dfs graph [1]
      tree' = countNode tree
      tree'' = (\x -> if even x then 1 else 0) <$> tree' :: Tree Int
  putStrLn . drawTree $ show <$> tree
  putStrLn . drawTree $ show <$> tree''
  print . getSum $ foldMap Sum tree'' - Sum (rootLabel tree'')

g1 = [
  (2,1),
  (3,1),
  (4,3),
  (5,2),
  (6,1),
  (7,2),
  (8,6),
  (9,8),
  (10,8)]
  -}
-- kadoban@hackerrank
{-
{-# LANGUAGE BangPatterns #-}
module Main
( main
) where

import           Prelude hiding (interact, lines, unlines, words)
import           Data.Text (pack, unpack, Text, lines, words)
import           Data.Text.IO (interact)
import           Data.Monoid ((<>))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List (foldl')

type N = Int
type Tree = IntMap [N]

toTree :: [(N, [N])] -> Tree
toTree = M.fromListWith (<>)

numEvenSubs :: Tree -> N
numEvenSubs t = subtract 1 . snd $ go 1
    where go :: N -> (N, N)
          go = checkCut . foldl' combine (1, 0) . map go . children
          combine (!a, !b) (!c, !d) = (a+c, b+d)
          checkCut ab@(a, b) | a `mod` 2 == 0 = (a, b+1)
                             | otherwise      = ab
          children n = M.findWithDefault [] n t

handleInput :: Text -> Text
handleInput = lines & drop 1 & map handleLine -- now [[N]]
            & toTree & numEvenSubs            -- now N
            & show & pack                     -- now Text
    where handleLine = words & map toInt & coerce
          coerce [a, b] = (b, [a])
          coerce _      = error "input error"
          toInt :: Text -> N
          toInt = unpack & read

main :: IO ()
main = interact handleInput

(&) :: (a -> b) -> (b -> c) -> a -> c
(&) = flip (.)
infixl 9 &
-}
--
-- Snake and Ladders
{-
import qualified Data.ByteString.Char8 as B
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Control.Monad
import Data.Functor
-- | Weighted Graph
type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b, (a,w))]) edges

dijkstra :: Graph Int -> S.Set (Int,Vertex) -> M.Map Vertex Int -> M.Map Vertex Int
dijkstra graph pq dist
  | S.null pq                  = dist
  | d_u' /= Nothing && d_u < d = dijkstra graph pq' dist
  | otherwise                  = dijkstra graph new_pq new_dist
  where ((d,u), pq') = S.deleteFindMin pq
        d_u' = M.lookup u dist
        d_u = case d_u' of {Just v -> v; Nothing -> 0}
        (new_pq, new_dist) = foldr go (pq', dist) [ (x,w) | (x,w) <- graph ! u ]
        go (x,w) (pq,dist)
          | d_x' == Nothing         = res
          | d_u + w < fromJust d_x' = res
          | otherwise               = (pq, dist)
          where d_x' = M.lookup x dist
                new_d = d_u + w
                res = (S.insert (new_d, x) pq, M.insert x new_d dist)

dijkstra' :: Graph Int -> Vertex -> M.Map Vertex Int
dijkstra' graph start = dijkstra graph (S.singleton (0,start)) (M.singleton start 0)

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  t <- readInt <$> B.getLine
  replicateM_ t solve
  
solve :: IO()
solve = do
  [n] <- rl
  ladders <- replicateM n $ do
    [from,to] <- rl
    return (from,to)
  [m] <- rl
  snakes <- replicateM m $ do
    [from,to] <- rl
    return (from,to)
  let graph = graphFromEdges (1,100) [(u,v',1) | u <- [1..99], v <- [u+1..u+6], v <= 100
                                               , u == check u
                                               , let v' = check v]
      check x = case lookup x ladders of
        Just v  -> v
        Nothing -> case lookup x snakes of
          Just v -> v
          Nothing -> x
  print . fromMaybe (-1) $ M.lookup 100 (dijkstra' graph 1)
  where rl = map readInt . B.words <$> B.getLine
{-
1
1
3 90
7
99 10
97 20
98 30
96 40
95 50
94 60
93 70
should input -1
-}
-}
-- Floyd: City of Blinding Light
{-
{-# LANGUAGE BangPatters #-}
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Control.Applicative
import Debug.Trace

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]
type AdjMatrix a = Array (Vertex,Vertex) a

buildAdjMatrix :: (Int, Int) -> [Edge Int] -> AdjMatrix (Maybe Int)
buildAdjMatrix (a,b) edges = accumArray (flip const) Nothing ((a,a),(b,b)) edges'
  where edges' = map (\(a,b,w) -> ((a,b),Just w)) edges

floydWarshall :: AdjMatrix (Maybe Int) -> AdjMatrix (Maybe Int)
floydWarshall am = traceShow "doing" $ runST $ do
  arr <- thaw am :: ST s (STArray s (Vertex,Vertex) (Maybe Int))
  sequence_ [ go arr k i j | k <- r, i <- r, j <- r]
  freeze arr
  where ((minb,_), (maxb,_)) = bounds am
        r = [minb..maxb]
        go :: STArray s (Vertex,Vertex) (Maybe Int)
           -> Vertex -> Vertex -> Vertex -> ST s ()
        go arr k i j = do
          ij <- readArray arr (i,j)
          ik <- readArray arr (i,k)
          kj <- readArray arr (k,j)
          case (ik, kj) of
            (Nothing, _) -> return ()
            (_, Nothing) -> return ()
            (Just a, Just b) -> case ij of
              Nothing  -> do
                writeArray arr (i,j) $ Just (a+b)
              (Just c) -> when (c > a+b) $ do
                writeArray arr (i,j) $ Just (a+b)
        myMin Nothing x = x
        myMin x Nothing = x
        myMin x y = min x y

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,m] <- rl
  edges <- replicateM m $ do
    [from,to,weight] <- rl
    return (from,to,weight)
  [q] <- rl
  let am = buildAdjMatrix (1,n) edges
      !res= floydWarshall am
  replicateM_ q $ do
    [start,end] <- rl
    putStrLn . show $ maybe (-1) id (res ! (start,end))
  where rl = map readInt . B.words <$> B.getLine
-}
--
{- quick_dudley@hackerrank
import Data.List
import Data.Maybe
import Data.Array.MArray
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST

floyd :: Int -> [(Int,Int,Integer)] -> Array (Int,Int) (Maybe Integer)
floyd n e = runSTArray $ do
  g <- newArray ((1,1),(n,n)) Nothing :: ST s (STArray s (Int,Int) (Maybe Integer))
  forM_ [1 .. n] $ \d -> writeArray g (d,d) (Just 0)
  forM_ e $ \(a,b,v) -> writeArray g (a,b) (Just v)
  forM_ [1 .. n] $ \k -> do
    forM_ (delete k [1 .. n]) $ \i -> do
      a' <- readArray g (i,k)
      case a' of
        Nothing -> return ()
        Just a -> forM_ ([1 .. n] \\ [k,i]) $ \j -> do
          b' <- readArray g (k,j)
          case b' of
            Nothing -> return ()
            Just b -> do
              d' <- readArray g (i,j)
              case d' of
                Nothing -> writeArray g (i,j) (Just $ a + b)
                Just d -> let
                  m = a + b
                  in if m < d
                    then writeArray g (i,j) (Just m)
                    else return ()
  return g

main = do
  ~(n:m:_) <- fmap (map read . words) getLine
  e <- fmap (map ((\ ~(a:b:v:_) -> (fromIntegral a, fromIntegral b, v)) . map read . words)) $ forM [1 .. m] $ const getLine
  let costs = floyd n e
  q <- fmap read getLine
  forM_ [1 .. q] $ const $ do
    ~(a:b:_) <- fmap (map read . words) getLine
    print $ maybe (-1) id (costs ! (a,b))
-}
{-
-- stephanj1gn@hackerrank

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Map as M

floyd2 n weights = do
  let at i j = i*(n+2) + j
  vec <- V.thaw $ V.fromList $ [ weights M.! (i,j) | i <- [0..n+1], j <- [0..n+1]]

  forM_ [1..n+1] $ \k ->
    forM_ [1..n+1] $ \i ->
      forM [1..n+1] $ \j -> do
        a <- VM.read vec (at i k)
        b <- VM.read vec (at k j)
        c <- VM.read vec (at i j)
        if c > a + b then
          VM.write vec (at i j) (a+b)
        else return ()
  V.freeze vec

buildWeights [] m = m
buildWeights ((i,j,w):edges) m = buildWeights edges $ M.insert (i,j) w m

inf = 999999999

main = do
  aa <- getLine
  let [n,m] = map read $ words aa :: [Int]
  bb <- replicateM m getLine

  let mappy = M.fromList $ [((i,j),k) | i <- [1..n+1], j <- [1..n+1], let k = if i ==j then 0 else inf]
  let edges = map (\[a,b,c] -> (a,b,c)) $ map (map read . words) bb :: [(Int,Int,Int)]

  let mappy' = buildWeights edges mappy

  q <- readLn :: IO Int
  cc <- replicateM q getLine
  let qs = map (\[a,b] -> (a,b)) $ map (map read . words) cc :: [(Int,Int)]

  ans <- floyd2 n mappy'

  let at i j = i*(n+2) + j
  let ans' = map (\a -> if a == inf then -1 else a) $ map (\(i,j) -> ans V.! (at i j)) qs


  putStrLn $ unlines $ map show ans'


--  print  $ mappy' M.! (311,312)
-}
{-
-- ashtefan@hackerrank
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
readInt s = let Just (a,_) = BS.readInt s in a

big = 10^9

buildGM n rs = runSTUArray $ do
  g <- newArray (0, n * n - 1) big
  forM_ (rs ++ map (\i->[i,i,0]) [1..n]) $ \[f,t,w] -> do
    let ind = (f - 1) * n + t - 1
    writeArray g ind w
  return g

floyd :: Int -> UArray Int Int -> UArray Int Int
floyd n g = runSTUArray $ do
  mg <- thaw g
  forM_ [0..n-1] $ \k -> do
    forM_ [0..n-1] $ \i -> do
      forM_ [0..n-1] $ \j -> do
        ij <- readArray mg (i * n + j)
        ik <- readArray mg (i * n + k)
        kj <- readArray mg (k * n + j)
        writeArray mg (i * n + j) $ min ij $ ik + kj
  return mg

solve ([n,m]:r) = map query qs
  where (gd,_:qs) = splitAt m r
        g = floyd n $ buildGM n gd
        query [f,t] = let v = g ! ((f - 1) * n + t - 1)
                      in if v >= big then (-1) else v

main = BS.interact $ BS.unlines . map (BS.pack . show) . solve . map (map readInt . BS.words) . BS.lines
-}
-- Journey to the Moon
{-
import Data.Bits
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.IO
import Data.Array((!), listArray, Array)
import Data.Array.Unboxed(UArray)
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List

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
  when (i /= j) $ do
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    traceShowM (szi,szj)
    if (szi < szj)
      then do writeArray (ids uf) i j
              writeArray (szs uf) j (szi + szj)
      else do writeArray (ids uf) j i
              writeArray (szs uf) i (szj + szi)

-- sizes of corresponding elements, for hackerrank
getSize :: (UnionFind s) -> ST s [Int]
getSize uf = do
  a <- getAssocs . ids $ uf
  b <- getElems . szs $ uf
  return . map snd . filter (\((k,v),_) -> k == v) $ zip a b

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,m] <- rl
  pairs <- replicateM m $ do
    [a,b] <- rl
    return (a,b)
  let l = runST $ do
        uf <- newUnionFind n
        mapM_ (uncurry $ unite uf) pairs
        getAssocs (ids uf) >>= traceShowM
        getAssocs (szs uf) >>= traceShowM
        getSize uf
  print $ show l
  print . sum $ zipWith (*) l (tail $ scanr1 (+) l)
  where rl = map readInt . B.words <$> B.getLine
        go [] = 0
        go [_]= 0
        go (x:xs) = x * sum xs
-}
--
{-
-- wood6@hackerrank
import Prelude hiding (words, lines, getContents)
import Data.List hiding (words, lines)
import Data.Text (Text, words, lines)
import Data.Text.IO (getContents)
import Data.Text.Read
import Data.Either
import Data.Graph
import Data.Tree

sums (x:xs) = zs where zs = x : zipWith (+) xs zs

count xs = sum $ zipWith (*) (sums xs) (tail xs)

solve (n,es) = count xs
  where
    g = buildG (0,n-1) es
    cs = components g
    xs = map (toInteger . length . flatten) cs

-------------------------------------------------------------------
read_Int = fst . g . decimal where g (Right x) = x

parse txt = (n, f xs)
  where (x:xs) = lines txt
        [n,l] = g 2 x
        f = map (p . g 2) . take l
        p [a,b] = (a,b)
        g n = map read_Int . take n . words

main = print . solve . parse =<< getContents
-------------------------------------------------------------------
-}
-- Jack Goes to Rapture
{-
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Control.Applicative
import Debug.Trace
--import Data.Monoid
import Data.Semigroup

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]
type AdjMatrix a = Array (Vertex,Vertex) a

buildAdjMatrix :: (Int, Int) -> [Edge Int] -> AdjMatrix (Maybe Int)
buildAdjMatrix (a,b) edges = accumArray (flip const) Nothing ((a,a),(b,b)) edges'
  where edges' = map (\(a,b,w) -> ((a,b),Just w)) edges

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b,(a,w))]) edges

dgraphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
dgraphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = map (\(a,b,w) -> (a, (b,w))) edges

primMST :: Graph Int -> Vertex -> Graph Int
primMST graph start = dgraphFromEdges (bounds graph) $ prim' pq taken []
  where (pq, taken) = process start S.empty S.empty
        process vertex pq taken = (pq', taken')
          where taken' = S.insert vertex taken
                pq'    = foldr S.insert pq [ (w, (vertex,x)) | (x,w) <- graph ! vertex
                                                             , not (S.member x taken') ]
                -- sort by (inc) weight then by (inc) id
        prim' pq taken mst
          | S.null pq         = mst
          | S.member to taken = prim' pq' taken mst
          | otherwise         = let (pq'', taken') = process to pq taken
                                in prim' pq'' taken' $ (from,to,weight):mst
          where ((weight, (from,to)), pq') = S.deleteFindMin pq

treeDfs :: Graph Int -> Vertex -> Vertex -> Maybe Int
treeDfs tree start end = getMax <$> lookup end (go start $ Max 0)
  where go node acc = concat [ (v, acc <> Max w):go v (acc <> Max w) | (v,w) <- tree!node]

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,e] <- rl
  edges <- replicateM e $ do
    [a,b,w] <- rl
    return (a,b,w)
  let graph = graphFromEdges (1,n) edges
      res= primMST graph 1
  print $ treeDfs res 1 n
  where rl = map readInt . B.words <$> B.getLine

g2 = [
  (0,1,4),
  (0,2,4),
  (3,0,6),
  (0,4,6),
  (1,2,2),
  (2,3,8),
  (3,4,9)]
test3 x y = treeDfs (flip primMST x $ traceShowId $ graphFromEdges (0,4) g2) x y
-}
-- Matrix
{-
import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Control.Monad
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Data.Semigroup

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]

big :: Int
big = 10^9

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b,(a,w))]) edges

dgraphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
dgraphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = map (\(a,b,w) -> (a, (b,w))) edges

dfs :: Graph Int -> S.Set Vertex -> Vertex  -> Int
dfs graph target start = sum . map getMin . fst $ go (Min big) (start,0) ([], S.empty)
  where go :: (Min Int) -> (Vertex,Int) -> ([Min Int], S.Set Vertex) -> ([Min Int], S.Set Vertex)
        go minv (node,w) (acc, visited)
          | traceShow (minv,(node,w),(acc,visited)) False = undefined
          | S.member node visited = (acc, visited)
          | S.member node target  = let (acc2, visited2) = foldr (go (Min big)) ([], visited') children
                                    in trace (">>" ++ show (node,newmin) ++ "<<") (newmin:acc ++ acc2, visited2)
          | otherwise             = let (acc2, visited2) = foldr (go newmin) ([], visited') children
                                    in (acc ++ acc2, visited2)
          where visited' = S.insert node visited
                children = [ (v,w) | (v,w) <- graph!node
                                   , not (S.member v visited) ]
                newmin = minv <> Min w
{-
dfs :: Graph Int -> S.Set Vertex -> Vertex  -> Int
dfs graph target start = sum $ go S.empty (Min 0) start
  where go visited acc node
          | S.member node target = getMin acc: concatMap (\(v,w) -> go visited' (Min w) v) children
          | otherwise            = concatMap (\(v,w) -> go visited' (acc <> Min w) v) children
          where visited' = S.insert node visited
                children = [ (v,w) | (v,w) <- graph!node
                                   , not (S.member v visited) ]
-}
readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,k] <- rl
  edges <- replicateM (n-1) $ do
    [a,b,w] <- rl
    return (a,b,w)
  t <- replicateM k (rl >>= return . head)
    
  let graph = graphFromEdges (0,n-1) edges
      target = S.fromList t
  print $ dfs graph target (head t)
  where rl = map readInt . B.words <$> B.getLine

g2 = [
  (0,1,4),
  (0,2,4),
  (3,0,6),
  (0,4,6)]
test = dfs (graphFromEdges (0,4) g2) (S.fromList [1,4]) 1
-}
-- Jeanine's Route

-- | go returns (1) 0 if no targets reached
-- |            (2) sum of the weight of the edges to reach target
import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Control.Monad
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Data.Semigroup
import Data.List (foldl')

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]
type MyState = S.Set Vertex

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b,(a,w))]) edges

dfs :: Graph Int -> [Vertex] -> Vertex -> Int
dfs graph t start = fst $ go (-1) (0, (S.empty)) (start,0)
  where targets = S.fromList t
        go :: Vertex -> (Vertex,Int) -> Int
        go parent (v,w) 
          | traceShow (acc, remain, (v,w)) False = undefined
          | 
        -- | this node
          | remain' == S.empty = (acc + w, remain') -- not backtrack. bucks stop here
          | null children      = (acc, remain')     -- backtrack with
        -- | children
          | remain'' == S.empty = (acc + w + childAcc, remain'') -- bucks stop here
          | otherwise        = (acc + (2*w) + childAcc, remain'')
          where remain' = S.delete v remain
                children = [ (v,w) | (v,w) <- graph!v, v /= parent ]
                (childAcc, remain'')
                  = foldl' (go v) children

graph = [
  (1,2,2),
  (2,4,2),
  (2,3,2),
  (3,5,3)]
g2 = [
  (1,2,1),
  (2,3,10)]
test = dfs (graphFromEdges (1,5) graph) [1,3,4] 1
t2 = dfs (graphFromEdges (1,3) g2) [1,3] 2

