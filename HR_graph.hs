import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector)

type Node = Int
type Edge = (Node, Node)
type Graph = (Int, [Edge], Node) -- number of vertex, edge list, start node

readGraphs :: [String] -> [Graph]
readGraphs [] = []
readGraphs (h:t) = (n, edge', read s) : readGraphs remain
  where [n,m] = map read . words $ h
        (edge,s:remain) = splitAt m t
        edge' = map ((\[x,y] -> (x,y)) . map read . words)  edge

adj :: [Edge] -> Node -> [Node]
adj edges node = foldl' (\acc (x,y) -> if x == node then y:acc
                                       else if y == node then x:acc
                                            else acc) [] edges
  
shortestReach :: Graph -> [Int]
shortestReach g@(num, edges, start) = shortestReach' (V.fromList initDist) g
  where initDist = replicate (start - 1) (-1) ++ (0: replicate (num - start) (-1))

shortestReach' :: Vector Int -> Graph -> [Int]
shortestReach' dists (num, edges, start) =
  map (\x -> if x > 0 then x * 6 else x) . filter (/= 0) . V.toList . fst . 
  fromJust . find (\(x,y) -> x == y) . zip distList $ (tail distList)
  where update _ (i,d) | d /= (-1) = d
        update distMap (i,_) = case filter (>= 0) . map ((distMap V.!) . pred) $ (adj edges i) of
          [] -> -1
          l  -> succ $ minimum l
        distList = iterate (\d -> V.map (update d) . V.zip (V.fromList [1..num]) $ d) dists
{-        
baz :: Vector Int -> Graph -> [Vector Int]
baz dists (num, edges, start) =  distList
--  fromJust . find (\(x,y) -> x == y) . zip distList $ (tail distList)
  where update _ (i,d) | d /= (-1) = d
        update distMap (i,_) = case filter (>= 0) . map ((distMap !!) . pred) $ (adj edges i) of
          [] -> -1
          l  -> succ $ minimum l
        distList = iterate (\d -> map (update d) . zip [1..num] $ d) dists
-}
sample = "4 3\n1 2\n2 3\n3 4\n1"
foo = map (unwords . map (show :: Int->String) . shortestReach) . readGraphs . lines $ sample
bar = readGraphs . lines $ sample

main = do
    nrTest <- readLn :: IO Int
    inputdata <- getContents
    mapM_ (putStrLn . unwords . map (show :: Int->String) . shortestReach) . readGraphs . lines $ inputdata

