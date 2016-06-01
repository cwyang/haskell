{-
-- Jumping on the Cloud
go :: [Int] -> Int
go [] = 0
go [0] = 0
go (0:_:0:xs) = 1 + go (0:xs)
go (0:0:xs) = 1 + go (0:xs)
main = getLine >> getLine >>=
  print . go . map read . words

-- Beautiful Thriplets
import Data.List
go :: Int -> [Int] -> Int
go _ [] = 0
go _ [x] = 0
go _ [x,y] = 0
go d (x:xs) = if elem (x+d) xs
              then if elem (x+2*d) xs
                   then 1 + go d xs
                   else go d xs
              else go d xs
main = do
  l1 <- getLine
  l2 <- getLine
  let [_,d] = map read . words $ l1
      l = map read . words $ l2
  print . go d $ l
-}
  
{-
-- Yet Another KMP

-- 2,2 => aabb
-- 2,3 => aabbb
-- 3,3 => aababb
-- 3,4 => aababbb
-- 3,5 => aababbbb
-- 4,4 => aabababb
-- 4,5 => aabababbb
import Data.List
import Data.Function
kmp :: [Int] -> String
kmp l
  | null s    = genStr l'
  | v == 1    = c:s
  | v == 2    = if c < head s
                then [c,c] ++ s
                else c:genStr2 s [c]
  | v == 3    = if c < head s
                then [c,c] ++ [head s]  ++ insertString (tail s) [c]
                else [c] ++ [head s] ++ insertString (tail s) [c,c]
  | otherwise = if c < head s
                then [c,c] ++ [head s] ++ genStr2 (tail s) (replicate (v-2) c)
                else [c] ++ [head s] ++ insertString (tail s)  (replicate (v-1) c)
  where l' = sort . filter ((/= 0) . fst) . zip l $ ['a'..]
        (v,c) = head $ l'
        l'' = sortBy (compare `on` (\(x,y) -> (y,x))) $ tail l'
        s = genStr l''
        insertString l p = let (left, right) = break (> head p) l
                           in left ++ p ++ right
genStr :: [(Int, Char)] -> String
genStr [] = []
genStr ((n,char):next) = replicate n char ++ genStr next

genStr2 :: String -> String -> String
genStr2 l p
--  | null left = joinStr l p
  | null right = reverse $ joinStr (reverse p) (reverse l)
  | lright >= lp = left ++ joinStr p right
  | otherwise = reverse (joinStr (reverse left) (reverse (take (lp - lright) p)))
    ++ joinStr (take lright p) right
  where (left, right) = break (> head p) l
        lp = length p
        lleft = length left
        lright = length right
joinStr :: String -> String -> String
joinStr [] x = x
joinStr x [] = x
joinStr (x:xs) (y:ys) = x:y:joinStr xs ys

-- for reference
kmp' :: [Int] -> String --[(Int,String)]
kmp' l = snd . head .sort $ sc
  where l' = sort . filter ((/= 0) . fst) . zip l $ ['a'..]
        ls = genAll $ genStr l'
        sc = map (\x -> (score x, x)) ls
        score x = let x' = reverse . tail . inits $ x
                      go y = let i = tail . reverse . inits $ y
                                 j = tail . tails $ y
                             in case find (\(x,y) -> x == y) (zip i j) of
                                  Nothing -> 0
                                  Just (x,_)  -> length x
                  in sum (map go x')
genAll :: Eq a => [a] -> [[a]]
genAll [] = [[]]
genAll l = concat [ map (x:) (genAll (delete x l)) | x <- l ]
test::[[Int]]
test=[[w,x,y,z] | w <- [1..8], x <- [1..8], y<-[1..8], z<-[1..8], w+x+y+z == 8]

main = getLine >>= putStrLn . kmp . map read . words
-- Gena playing Hanoi
-- 4 rod hanoi: ==> use bfs
-- [1,2,3] - - []
--
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.Maybe
import qualified Data.Map as M

type Move = Int
type Position = [Int]
type Path = ([Position], Position)
type Frontier = [Path]

solved :: Position -> Bool
solved = all (== 1)
succs :: Path -> [Path]
succs (ms, pos) = [ (newpos:ms, newpos) | newpos <- concatMap (go pos) [1..length pos]]
  where cand n | (pos !! (n-1)) `elem` take (n-1) pos = []
--               | all (== 1) (drop (n-1) pos) = []
               | otherwise = [1..4] \\ take n pos
        go :: Position -> Int -> [Position]
        go pos n = map (\x -> take (n-1) pos ++ x:drop n pos) (cand n)
--start :: Position
--start = [2,2,1,1] -- one move then done
-- when start is over 8disc, bfs performance crumbles
encode :: Position -> Int
encode = foldl' (\acc x -> acc * 5 + x) 0
hash :: Position -> Int
hash p = encode p `mod` 10111

solve :: Position -> Maybe [Position]
solve start = runST $
  do pa <- newArray (0,10110) M.empty
     bfs pa [([],start)] []
bfs :: STArray s Int (M.Map Int Position) -> Frontier -> Frontier -> ST s (Maybe [Position])
bfs pa [] [] = return Nothing
bfs pa [] mqs = bfs pa mqs []
bfs pa ((ms,p):mps) mqs
  | solved p  = return (Just (reverse ms))
  | otherwise = do pmap <- readArray pa k
                   if M.member e pmap
                     then bfs pa mps mqs
                     else do writeArray pa k (M.insert e p pmap)
                             bfs pa mps (succs (ms,p) ++ mqs)
  where e = encode p
        k = hash p

main = getLine >> getLine >>=
  print . length . fromJust . solve . map read . words
-}

{-
-- Move The Coin
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.List
import qualified Data.Map.Strict as M

genTree :: Int -> [Int] -> [(Int,Int)] -> Array Int (Int,Int)
genTree n l edges = runST $
  do xa <- newListArray (1,n) (zip l (repeat 0))        -- Val, Parent
     mapM_ (addEdge xa) edges
     freeze xa

genTree' :: Int -> [Int] -> [(Int,Int)] -> [(Int,Int)]
genTree' n l edges = runST $
  do xa <- newListArray (1,n) (zip l (repeat 0))        -- Val, Parent
     let sedges = sortEdge (M.singleton 1 1) [] edges
     mapM_ (addEdge xa) edges
     getElems xa

addEdge :: STArray s Int (Int,Int) -> (Int,Int) -> ST s ()
addEdge xa (parent,child) = do (v,_) <- readArray xa child
                               writeArray xa child (v,parent)

sortEdge :: M.Map Int Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
sortEdge _ s [] = s
sortEdge visitedNode sortedEdges edges =
  sortEdge vn2 (e1++e2'++sortedEdges) e3
  where (e1,e') = partition (\(x,y) -> M.member x visitedNode) edges
        (e2,e3) = partition (\(x,y) -> M.member y visitedNode) e'
        e2' = map (\(x,y) -> (y,x)) e2
        vn1 = foldl' (\acc (_,x) -> M.insert x 1 acc) visitedNode e1
        vn2 = foldl' (\acc (_,x) -> M.insert x 1 acc) vn1 e2'

bar = sortEdge (M.singleton 1 1) [][(2,1),(1,3),(3,4),(5,3),(4,6)]
foo = genTree 6 [0,3,2,2,2,2] [(1,2),(1,3),(3,4),(3,5),(4,6)]

go :: Int -> Array Int (Int,Int) -> (Int,Int) -> String
go n oa (node,newparent) =
  let (v,_)  = oa ! node
      na = oa // [(node,(v,newparent))]
  in if checkParent n na node == False
     then "INVALID"
     else if decide n na == True
          then "YES"
          else "NO"
checkParent :: Int -> Array Int (Int,Int) -> Int -> Bool
checkParent 0 _ _ = False
checkParent n arr node = let (_,parent) = arr ! node in
  if parent == 1 then True
  else checkParent (n-1) arr parent

decide :: Int -> Array Int (Int,Int) -> Bool
decide n na = if odd . sum . map go $ [2..n] then True
              else False
  where go x = let (v,_) = na ! x
                   lv = level x
               in if even v then 0
                  else v * lv
        level 1 = 0
        level x = let (_,p) = na ! x
                  in 1 + level p
main = do
  l1 <- getLine
  l2 <- getLine
  let n = (read ::String->Int) l1
      node = map read . words $ l2
  l3 <- mapM (\_ ->getLine) [1..n-1]
  let edge = map ((\[x,y]->(x,y)) . map read . words) l3
  l4 <- getLine
  let q = (read :: String->Int) l4
  l5 <- mapM (\_ ->getLine) [1..q]
  let qs = map ((\[x,y]->(x,y)) . map read . words) l5
      tree = genTree n node edge
  mapM_ (putStrLn. go n tree) qs
-}
-- Sum of the Maximums
import Data.List

go :: [Int] -> (Int,Int) -> Int
go arr (l,r) = let arr' = take (r-l+1) $ drop (l-1) arr
                   arr'' = init . subarray $ arr'
               in sum $ map maximum arr''
subarray :: [Int] -> [[Int]]
subarray [] = [[0]]
subarray arr = let v = tail . inits $ arr
               in v ++ subarray (tail arr)

powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

main = do
  l1 <- getLine
  l2 <- getLine
  let [n,q] = map (read ::String->Int) . words $ l1
      l = map read . words $ l2
  l3 <- mapM (\_ -> getLine) [1..q]
  let query = map ((\[x,y]->(x,y)) . map read . words) l3
  mapM_ (putStrLn . show . go l) query
