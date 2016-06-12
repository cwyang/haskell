{-
-- Connected Cell in a grid
import Data.Array
import Data.List
import Debug.Trace

type Pos = (Int,Int)

adj :: Array Pos Int -> Pos -> [Pos]
adj arr (rid,cid) = [ (x,y) | (x,y) <- [(rid-1,cid), (rid+1,cid), (rid,cid-1), (rid,cid+1),
                                        (rid-1,cid-1), (rid+1,cid-1), (rid+1,cid-1), (rid+1,cid+1)],
                      x >= 0 && y >= 0 && x <= rows && y <= cols]
  where (_,(rows,cols)) = bounds arr

findArray :: Array (Int,Int) Int -> Int -> Maybe (Int,Int)
findArray arr v = fmap fst . find ((==v) . snd) $ assocs arr

toArray rows cols l = listArray ((0,0), (rows-1,cols-1)) l

dfs :: Array Pos Int -> Int -> [Pos] -> (Int, Array Pos Int)
--dfs a b c | trace ("dfs " ++ show a ++ show b ++ show c) False = undefined
dfs arr n [] = (n, arr)
dfs arr n (current:path) = let arr' = arr // [(current, 0)]
                               sibling = [ pos | pos <- adj arr current, arr' ! pos == 1 ]
                           in dfs arr' (n+ arr!current) (sibling ++ path)

go :: [[Int]] -> Int
go l = go' 0 arr
  where arr = toArray (length l) (length $ head l) $ concat l
        go' record arr = case findArray arr 1 of
          Nothing -> record
          Just idx  -> let (v,arr') = dfs arr 0 [idx]
                       in go' (max record v) arr'

-}
-- Count Luck
import Data.Array
import Data.List
import Debug.Trace

type Pos = (Int,Int)

choice :: Array Pos Char -> Pos -> [Pos]
choice arr (rid,cid) = [ (x,y) | (x,y) <- [(rid-1,cid), (rid+1,cid), (rid,cid-1), (rid,cid+1)],
                      x >= 0 && y >= 0 && x <= rows && y <= cols,
                      arr ! (x,y) == 'X' && arr ! (x,y) == 'M']
  where (_,(rows,cols)) = bounds arr

findArray :: Array Pos Char -> Char -> Maybe Pos
findArray arr v = fmap fst . find ((==v) . snd) $ assocs arr

toArray rows cols l = listArray ((0,0), (rows-1,cols-1)) l

dfs :: Array Pos Char -> [(Int,Pos)] -> Int
--dfs a b c | trace ("dfs " ++ show a ++ show b ++ show c) False = undefined
dfs arr ((n,current):path)
  | arr ! current == '*' = n
  | otherwise = dfs arr' (c ++ path)
  where arr' = arr // [(current, 'X')]
        c = [ (n+1,pos) | pos <- choice arr current ]

go :: [[Char]] -> Int
go l = go' 0 arr
  where arr = toArray (length l) (length $ head l) $ concat l
        go' record arr = case findArray arr 'M' of
          Nothing -> undefined
          Just idx  ->  dfs arr [(0,idx)]
