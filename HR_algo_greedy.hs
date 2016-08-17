{-
-- Max Min
import Data.List

data Queue a = Queue [a] [a]
empty :: Queue a
empty = Queue [] []
push :: a -> Queue a -> Queue a
push a (Queue x y) = Queue (a:x) y
pop :: Queue a -> (a, Queue a)
pop (Queue x (y:ys)) = (y, Queue x ys)
pop (Queue [] []) = error "cannot pop from empty queue"
pop (Queue x []) = pop (Queue [] (reverse x))
lastQ :: Queue a -> a
lastQ (Queue (a:x) _) = a
lastQ (Queue [] []) = error "empty queue"
lastQ (Queue [] x) = lastQ (Queue (reverse x) [])
qToList :: Queue a -> [a]
qToList (Queue x y) = y ++ reverse x

qVal :: Queue Int -> Int
qVal q = lastQ q - fst (pop q)

scanList :: Int -> [Int] -> Int
scanList k l = let (ini, rst) = splitAt k l
                   initQ = foldl' (\acc x -> push x acc) empty ini
                   go (minQ,curQ) x = let (_,nextQ) = pop $ push x curQ
                                      in if (qVal minQ > qVal nextQ)
                                         then (nextQ, nextQ)
                                         else (minQ, nextQ)
               in qVal $ fst $ foldl' go (initQ, initQ) rst
-}      
{-
-- Cutting Boards
import Data.List

calc :: Integer -> Integer -> Integer -> ([Integer],[Integer]) -> [Integer]
calc acc nx ny ([],[]) = [acc]
calc acc nx ny (cx:cxs,[]) = calc (acc+cx*ny) (nx+1) ny (cxs,[])
calc acc nx ny ([],cy:cys) = calc (acc+cy*nx) nx (ny+1) ([],cys)
calc acc nx ny (cx:cxs,cy:cys)
  | cx > cy = xcut -- very surprising greedy rules!
  | cx < cy = ycut
  | otherwise = xcut
  where xcut = calc (acc+cx*ny) (nx+1) ny (cxs, cy:cys)
        ycut = calc (acc+cy*nx) nx (ny+1) (cx:cxs, cys)

readArgs :: [String] -> [([Integer],[Integer])]
readArgs [] = []
readArgs (x:y:z:xs) =
  let mr = map read . words
      [m,n] = mr x
      y' = reverse $ sort $ mr y
      x' = reverse $ sort $ mr z
  in (y',x'):readArgs xs

main = getLine >> getContents >>=
  mapM_ (putStrLn . show . (flip rem $ 10^9+7) . head . sort . calc 0 1 1) . readArgs . lines
-- Chief Hopper
calc :: Int -> [Int] -> Int
calc energy [] = energy
calc energy (x:xs)
  | energy < x = calc (energy + ((x-energy+1)`div`2)) xs
  | energy > x = calc (x + ((energy-x+1) `div` 2)) xs
  | otherwise = calc energy xs
  
main = getLine >> getContents >>=
  print . calc 0 . reverse . map (read :: String ->Int). words
-}
-- Largest Permutation
{-
go :: Int -> [Int] -> [Int]
go 0 x = x
go _ [] = []
go k l@(x:xs) = let m = maximum l
                    (a,b) = break (== m) xs in
                if m == x then x : go k xs
                else m : go (k-1) (a++ (x: (tail b)))
main = do
    l1 <- getLine
    l2 <- getLine
    let [_,k] = map read . words $ l1
        l = map read . words $ l2
    putStrLn . unwords . map show . go k $ l
--
go :: Int -> Int -> [Int] -> [Int] -> [Int]
go _ _ _ [] = []
go 0 idx replaced l = go' idx (reverse replaced) l
  where go' idx replaced l@(x:xs) = 
    
go k idx replaced l@(x:xs)
  | x == idx  = idx : go k (idx-1) replaced xs
  | otherwise = idx : go (k-1) (idx-1) (x:replaced) xs
  
goRef k l@(x:xs) = let m = maximum l
                       (a,b) = break (== m) xs in
                   if m == x then x : go k xs
                   else m : go (k-1) (a++(x:(tail b)))
-}
-- Largest Permutation 2nd
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import qualified Data.Vector as V
import Data.Vector ((!), (//), Vector) -- Note that V.cons is O(n)
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf

calc :: M.IntMap Int -> M.IntMap Int -> Int -> Int -> Int -> M.IntMap Int
calc ntop pton n k idx
--  | trace (printf ">> %s %s %d %d %d" (show ntop) (show pton) n k idx) False = undefined
  | n == 0    = pton
  | k == 0    = pton
  | v == n    = calc ntop pton (n-1) k (idx+1)
  | otherwise = let p     = fromJust $ M.lookup (n-1) ntop       -- find n's pos
                    ntop' = M.insert (v-1) p ntop -- put vh to that pos
                    pton' = M.insert p v $ M.insert idx n pton
                in calc ntop' pton' (n-1) (k-1) (idx+1)
  where v = fromJust $ M.lookup idx pton
        
main = do
  [n,k] <- rl
  l <- rl
  putStrLn . unwords . map show . M.elems $ calc (ntop l) (pton l) n k 0
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        ntop l = M.fromList $ zip [0..] . map snd . sort $ zip l [0..]
        pton l = M.fromList $ zip [0..] l

