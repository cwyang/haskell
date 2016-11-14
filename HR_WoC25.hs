-- Between Two Sets
{-
import Data.Functor
import Debug.Trace
numFactors :: Int -> Int -> Int
numFactors d n
  | n == 1         = 1
  | d * d > n      = 2
  | n `mod` d /= 0 = numFactors (d+1) n
  | otherwise      = let a = length . takeWhile (\(_,y) -> y == 0) . iterate go $ (n`div`d,0)
                         go (x,_) = x `divMod` d                 
                     in (a+1) * numFactors (d+1) (n `div` d^a)

main = getLine >> do
  a <- map read . words <$> getLine
  b <- map read . words <$> getLine
  let l = foldr1 lcm a
      g = foldr1 gcd b
  print $ go l g
  where go l g | g `mod` l /= 0 = 0
               | otherwise      = numFactors 2 $ g `div` l

foo d n
  | otherwise      = takeWhile (\(_,y) -> y == 0) . iterate go $ (n`div`d,0)
  where go (x,_) = x `divMod` d                 
-}
-- Baby Step
{-
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Functor
import Control.Monad

solve :: (Int,Int,Int) -> Int
solve (a,b,x)
  | x == a = 1    -- short
  | r == 0 = d
  | x < b  = 2
  | otherwise = d + 1
  where (d,r) = x `divMod` b
        
main :: IO ()
main = do
  [q] <- rl
  queries <- replicateM q $ do
    [a,b,d] <- rl
    if a > b then return (b,a,d)
                  else return (a,b,d)
  mapM_ (print . solve) queries
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
-}
-- Stone Division
{-
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Functor
import Control.Monad
import Debug.Trace
import Control.Monad.ST
import Data.STRef
import qualified Data.IntMap.Strict as M
import Data.List
import Control.Monad.State

type MT' = M.IntMap Bool
calc''' :: STRef s MT' -> [Int] -> Bool -> Int -> ST s Bool
calc''' mc s firstTurn n
--  | traceShow (s,firstTurn,n) False = undefined
  | firstTurn && nextS == [] = return False -- lose
  | firstTurn                = (or <$>). forM nextN $ \x -> do
      m <- readSTRef mc
      case M.lookup x m of
        Just v -> return v
        Nothing -> do
          r <- calc''' mc nextS False x
          writeSTRef mc $ M.insert x r m
          return r
  | nextS == [] = return True -- win
  | otherwise   = (and <$>). forM nextN $ \x -> do
      m <- readSTRef mc
      case M.lookup x m of
        Just v -> return v
        Nothing -> do
          r <- calc''' mc nextS False x
          writeSTRef mc $ M.insert x r m
          return r
  where
    nextS =filter ((== 0) . mod n) s
    nextN = map (div n) nextS


calc :: [Int] -> Bool -> Int -> Bool -- rv: first's win
calc s firstTurn n -- First turn
--  | traceShow (s,firstTurn,n) False = undefined
  | firstTurn && nextS == [] = False -- win
  | firstTurn                = or $ [ calc nextS False x | x <- nextN ]
  | nextS == []              = True -- lose
  | otherwise                = and $ [ calc nextS True x | x <- nextN ]
  where
    nextS =filter ((== 0) . mod n) s
    nextN = map (div n) nextS

calc' :: M.IntMap Bool -> [Int] -> Bool -> Int -> (Bool, M.IntMap Bool) -- rv: first's win
calc' m s firstTurn n -- First turn
--  | traceShow (s,firstTurn,n) False = undefined
  | Just val <- v            = if firstTurn then (val, m)
                               else (not val, m)                                 
  | firstTurn && nextS == [] = (False, M.insert n False m) -- lose
  | firstTurn                = let (r,m') = foldl' (findAny nextS) (False, m) [ x | x <- nextN ]
                               in (r, M.insert n r m')
  | nextS == []              = (True, M.insert n False m) -- win
  | otherwise                = let (r,m') = foldl' (checkAll nextS) (True, m) [ x | x <- nextN ]
                               in (r, M.insert n (not r) m')
  where
    nextS =filter ((== 0) . mod n) s
    nextN = map (div n) nextS
    v = M.lookup n m
    findAny _ (True,m) _ = (True,m)
    findAny s (acc,m) x  = calc' m s False x
    checkAll _ (False,m) _ = (False, m)
    checkAll s (acc,m) x   = calc' m s True x

type MT = M.IntMap Bool
calc'' :: [Int] -> Bool -> Int -> State MT Bool
calc'' s firstTurn n = do
  let nextS =filter ((== 0) . mod n) s
      nextN = map (div n) nextS
  v <- gets $ M.lookup n
  case (v, firstTurn, null nextS) of
    (Just val, True, _)  -> return val
    (Just val, False, _) -> return $ not val
    (_, True, True)      -> do
      modify $ M.insert n False
      return False
    (_, True, False)     -> do
      r <- or <$> forM nextN (calc'' nextS False)
      modify (M.insert n r)
      return r
    (_, False,True)      -> do
      modify $ M.insert n False
      return True
    (_, False,False)     -> do
      r <- and <$> forM nextN (calc'' nextS True)
      m' <- get
      put $ M.insert n (not r) m'
      return r

main' :: IO ()
main' = do
  [n,m] <- rl
  s' <- rl
  let s = sort $ nub s'
{-
  let n = 10^10
      m = 1
      s = sort $ nub $ [5,2,3,4,7,10,3,4]
-}  
  if fst $ calc' M.empty s' True n then putStrLn "First(fold)"
    else putStrLn "Second(fold)"
  if flip evalState M.empty $ calc'' s' True n then putStrLn "First(state)"
    else putStrLn "Second(state)"
  if calc s' True n then putStrLn "First"
    else putStrLn "Second"
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
test x = fst $ calc' M.empty [5,2,3,4,7,10,34] True x
test2 x = flip evalState M.empty $ calc'' [5,2,3,4,7,10,34] True x
test3 x =  calc [5,2,3,4,7,10,34] True x
foo n f = map f $ take 10 $ iterate (*2) n

solve :: [Int] -> Bool -> Int -> Int -> Bool -- rv: first's win
solve s firstTurn n num -- First turn
--  | traceShow (s,firstTurn,n) False = undefined
  | firstTurn && nextS == [] = False
  | nextS == []              = True
  | firstTurn                = let r = or [ solve nextS False x num | (x,num) <- nextN ]
                               in if odd num then r else False
  | otherwise                = let r = and [ solve nextS True x num | (x,num) <- nextN ]
                               in if odd num then r else True
  where
    nextS =filter ((== 0) . mod n) s
    nextN = map (\x-> (div n x, x)) nextS



solve' :: M.IntMap Bool -> [Int] -> Bool -> Int -> Int -> (Bool, M.IntMap Bool) -- rv: first's win
solve' m s firstTurn n num -- First turn
--  | traceShow (s,firstTurn,n) False = undefined
  | firstTurn && nextS == [] = (False, M.insert n False m) -- lose
  | nextS == []              = (True, M.insert n True m) -- win
  | Just val <- v            = case (firstTurn, odd num) of
                               (True, True) -> (val, m)
                               (False, True) -> (not val, m)
                               (True, False) -> (False, m)
                               (False, False) -> (True, m)
  | firstTurn                = let (r,m') = foldl' (findAny nextS) (False, m) [ x | x <- nextN ]
                               in if odd num then (r, M.insert n r m') else (False, m')
  | otherwise                = let (r,m') = foldl' (checkAll nextS) (True, m) [ x | x <- nextN ]
                               in if odd num then (r, M.insert n (not r) m') else (True, m')
  where
    nextS =filter ((== 0) . mod n) s
    nextN = map (\x -> (div n x, x)) nextS
    v = M.lookup n m
    findAny _ (True,m) _ = (True,m)
    findAny s (acc,m) (x,num)  = solve' m s False x num
    checkAll _ (False,m) _ = (False, m)
    checkAll s (acc,m) (x,num)   = solve' m s True x num

main :: IO ()
main = do
  [n,m] <- rl
  s' <- rl
  let s = sort $ nub s'
--  if fst $ solve' M.empty s' True n 1 then putStrLn "First"
--    else putStrLn "Second"
  if solve s' True n 1 then putStrLn "First"
    else putStrLn "Second"
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
-}
-- DAG Queries
{-
{-# LANGUAGE BangPatterns #-}
import Data.Array
import Data.Array.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import Control.Monad
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Data.List

type Vertex  = Int
type Graph = Array Vertex (Int, [Vertex])
type IOGraph = IOArray Vertex (Int, [Vertex])
type Edge  = (Vertex, Vertex)

dgraphFromEdges :: (Int, Int) -> [Edge] -> Graph
dgraphFromEdges bounds edges = accumArray go (0,[]) bounds edges
  where go (x, vs) v = (x, v:vs) 

dgraphFromEdgesM :: (Int, Int) -> [Edge] -> IO IOGraph
dgraphFromEdgesM bounds edges = do
  arr <- newArray bounds (0,[])
  forM_ edges $ \(u,v) -> do
    (_,xs) <- readArray arr u
    writeArray arr u (0,v:xs)
  return arr
{-

trav :: Graph -> Vertex -> [Vertex]
trav graph node = node: concat [ v: trav graph v | v <- snd (graph ! node) ]
dfs :: Graph -> (Int -> Bool) -> Int -> Vertex -> Graph
dfs graph pred val start
  = array (bounds graph) $ assocs graph ++ map go (trav graph start)
  where go v = let (preval, vs) = graph ! v
               in (v, if pred preval then (val, vs) else (preval, vs))
-}
dfs :: IOGraph -> (Int -> Bool) -> Int -> Vertex -> IO ()
dfs graph pred val node = do
  (preval, children) <- readArray graph node
  let val' = if pred preval then val else preval
  if preval /= val' then do
    writeArray graph node (val', children)
   else
    return ()
  forM_  children $ \child -> do
    dfs graph pred val child

topoSort :: IOGraph -> IO [Vertex]
topoSort graph = do
  (start,end) <- getBounds graph
  visited <- newArray (start,end) False
  res <- forM [start..end] $ \i -> do
    visitedNode <- readArray visited i
    if visitedNode then
      return []
     else
      dfs2 visited i
  return $ concat (reverse res)
  where dfs2 :: IOArray Vertex Bool -> Vertex -> IO [Vertex]
        dfs2 visited node = do
          writeArray visited node True
          (_,children) <- readArray graph node
          res <- forM children $ \child -> do
            visitedChild <- readArray visited child
            if visitedChild then
              return []
             else
              dfs2 visited child
          return $ node: concat (reverse res)

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

optimize :: Array Int Int -> [(Int,Int,[Int])] -> [(Int,Int,[Int])]
optimize _ [] = []
optimize _ [x] = [x]
optimize topo ((3,a,b):xs) = (3,a,b): optimize topo xs
optimize topo ((_,a,b):(1,c,d):xs)
  | topo!c < topo!a = optimize topo $ (1,c,d):xs
optimize topo ((_,a,b):(2,c,d):xs)
  | (topo!c < topo!a) && d > b = optimize topo $ (2,c,d):xs
optimize topo (a:b:xs) = a: optimize topo (b:xs)

main :: IO ()
main = do
  [n,m,q] <- rl
  edges <- replicateM m $ do
    [a,b] <- rl
    return (a,b)
  arr <- dgraphFromEdgesM (1,n) edges
  topo <- topoSort arr
  print topo

  query <- replicateM q $ do
    (cmd:u:args) <- rl
    return (cmd, u, args)
  let query' = optimize (array (1,n) $ zip topo [1..]) query
  
  forM_ query' $ \(cmd,u,args) -> do
    case cmd of
      1 -> dfs arr (const True) (head args) u
      2 -> do
        let x = head args
        dfs arr (\a -> a > x) x u
      3 -> B.putStrLn . B.pack . show . fst =<< readArray arr u
  where rl = map readInt . B.words <$> B.getLine
-}
  {-
  forM_ [1..n] $ \i -> do
    (val,children) <- readArray arr i
    writeArray arr i (val, nub children)
  -}
  
{-
graph = [
  (1,2,2),
  (2,4,2),
  (2,3,2),
  (3,5,3)]
g2 = [
  (1,2,1),
  (2,3,10)]
test = dfs (dgraphFromEdges (1,5) graph) [1,3,4] 1
t2 = dfs (dgraphFromEdges (1,3) g2) [1,3] 2

-}
--
-- Minimal Cyclic Shift
import Data.Array
import Data.Array.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import Control.Monad
import Data.Maybe
import Control.Applicative
import Debug.Trace
import Data.List

m :: Int
m = 998244353

rotater :: Int -> [a] -> [a]
rotater _ [] = []
rotater n xs = zipWith const (drop n' (cycle xs)) xs
  where n' = length xs - n

rotaterList :: [a] -> [[a]]
rotaterList xs = init $ map (\x -> take l $ x ++ xs) (reverse . tails $ xs)
  where l = length xs

myNub :: [Int] -> [Int]
myNub [] = []
myNub [x] = [x]
myNub (x:y:xs)
  | x == y = myNub (y:xs)
  | otherwise = x : myNub (y:xs)

allSame :: [Int] -> Bool
allSame [] = True
allSame [_] = True
allSame (x:y:xs)
  | x == y    = allSame (y:xs)
  | otherwise = False

main :: IO ()
main = do
  [n,k] <- rl
  a <- rl
  b <- rl
  let cs = zip [0..n-1] . map (\x -> f k (a,x)) $ rotaterList b
--      f (as,bs) = length . myNub . sort $ zipWith (\a b -> (a - b) `mod` m) as bs
--      r = find (\(idx,v) -> v <= (k+1)) cs
      f k (as,bs)
        | ds == []               = True
        | allSame ds && head ds == 0  = True
        | allSame ds && k > 0    = True
        | otherwise              = False
        where ts =  zipWith (\a b -> (a - b) `mod` m) as bs
              ds = zipWith (-) ts (tail ts)
      r = find (\(idx,v) -> v) cs              
  case r of
    Nothing -> putStrLn "-1"
    Just (idx, _) -> print idx
  where rl = map readInt . B.words <$> B.getLine
        readInt = fst . fromJust . B.readInt
