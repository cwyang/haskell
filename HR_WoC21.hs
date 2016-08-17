{-
-- Kangaroo
go :: Int -> Int -> Int -> Int -> Bool
go x1 v1 x2 v2
  | x1 > x2              = go x2 v2 x1 v1
  | x1 == x2 && v1 == v2 = True
  | x1 == x2 && v1 /= v2 = False
  | v1 <= v2             = False
  | xd `rem` vd == 0     = True
  | otherwise            = False
    where xd = x2 - x1
          vd = v2 - v1
      
main = do
  [x1,v1,x2,v2] <- rl
  print $ if (go x1 v1 x2 v2) then "YES" else "NO"
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
{-
-- Luck Balance
import Data.List

main = do
  [n,k] <- rl
  l <- mapM (\_ -> fmap (\[a,b] -> (a,b)) rl) [1..n]
  let (imp,unimp) = partition (\(a,b) -> if b == 0 then False else True) l
      k' = length imp - k
      (win,lose) = splitAt k' $ sort $ map fst $ imp
  print $ (sum . map fst $ unimp) + sum lose - sum win
  where rl = fmap (map (read :: String->Int) . words) getLine
-- Lazy Sort --> Failed!
import Data.List
import Text.Printf
fact :: Int -> Integer
fact = (factTable !!)
factTable :: [Integer]
factTable = 1:zipWith (*) [1..] factTable

main = do
  [n'] <- rl
  p' <- rl
  let p = sort p'
      pg = group p
      n = fact n'
      c = foldl' (\acc x -> acc * fact(length x)) 1 pg
  if p == p'
    then putStrLn $ printf "%.6f" (0 ::Double)
    else putStrLn $ printf "%.6f" ((fromIntegral n / fromIntegral c) :: Double)
  where rl = fmap (map (read :: String->Int) . words) getLine
-- Demanding Money
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import qualified Data.IntMap.Strict as M

type Cost = Int
type Node = Int
type Way = Int

calc :: M.IntMap Cost -> M.IntMap [Node] -> [Node] -> (Cost, Way)
calc _ _ [] = (0, 1)
calc cost adj n@(nh:nt)
--  | trace (printf "calc %s %s %s" (show cost) (show adj) (show n)) False = undefined
  | null nt && c == 0 = (0,2)
  | null nt = (c, 1)
  | c1 > c2 = (c1, w1)    
  | c1 < c2 = (c2, w2)
  | otherwise =  (c1, w1+w2)
  where
    remain = nt \\ (fromMaybe [] $ M.lookup nh adj)
    (c1',w1) = calc cost adj remain
    c = getCost nh
    c1 = c1' + c
    (c2,w2)  = calc cost adj nt
    getCost = fromJust . flip M.lookup cost
    
main = do
  [n,m] <- rl
  c <- rl
  edges' <- mapM (\_ -> rl) [1..m]
  let edges = groupBy (\x y -> head x == head y) . sort $ map reverse edges' ++ edges'
      adj = M.fromList $ map (\x -> (head $ head x, map last x)) edges
      cost = M.fromList $ zip [1..] c
--      (a,b) = calc cost adj [1..n]
      nodes = map (\x -> let s = M.lookup x adj
                         in case s of
                           Nothing -> (x, 0)
                           Just s' -> (x, length s')) [1..n]
      (a,b) = calc cost adj (map fst . reverse $ sort nodes)
  putStrLn $ printf "%d %d" a b
  where rl = fmap (map (read :: String->Int) . words) getLine
-- Letter N
import Data.List
import Debug.Trace
type Point = (Int, Int)
data Loc = LeftP | RightP | CenterP deriving (Eq, Show)

determinant (ax,ay) (bx,by) (px,py)
  | d > 0 = LeftP
  | d < 0 = RightP
  | otherwise = CenterP
  where d = (bx - ax) * (py-ay) - (by-ay)*(px-ax)

partition' :: (a -> Loc) -> [a] -> ([a],[a])
{-# INLINE partition' #-}
partition' p xs = foldr (select p) ([],[]) xs
select :: (a -> Loc) -> a -> ([a],[a]) -> ([a],[a])
select p x ~acc@(ls, rs) | p x == CenterP = acc
                         | p x == RightP  = (ls, x:rs)
                         | otherwise      = (x:ls, rs)
        
acute, right, left :: Point -> Point -> Point -> Bool
-- acute = angle <= 90 degree
acute pa pc pb = 90 >= angle pa pc pb
angle pa pc pb
  | bc == 0 || ac == 0 = 0
  | otherwise = let v = acos ((bc^2+ac^2-ab^2) / (2*bc*ac)) / pi * 180
                in round' v
  where dist (x1,y1) (x2,y2) = sqrt ((fromIntegral $ x1-x2)^2 + (fromIntegral $ y1-y2)^2)
        ab = dist pa pb
        bc = dist pb pc
        ac = dist pa pc
        round' x = fromIntegral (round (x * 10^6)) / 10^6
right a b p = if determinant a b p == RightP then True else False
left a b p = if determinant a b p == LeftP then True else False

calc :: [Point] -> Int
calc points =
  sum [ length a' * length d'  | b' <- tails points, b' /= [], c <- tail b',
        let b = head b',
            b /= c,
        let (d,a) = partition' (determinant b c) points
            --let a = filter (right b c) points; d = filter (left b c) points
            a' = filter (acute c b) a
            d' = filter (acute b c) d ]
          where n = length points
calc' points =
  [ (a',b,c,d') | b' <- tails points, b' /= [], c <- tail b',
        let b = head b',
            b /= c,
        let (d,a) = partition' (determinant b c) points
            a' = filter (acute c b) a
            d' = filter (acute b c) d ]
          where n = length points

foo :: IO ()
foo = putStrLn $ unlines $ map show $ calc' [(0,0),(0,2),(2,0), (2,2)]
p1,p2,p3,p4::Point
p1 = (0,0)
p2 = (0,2)
p3 = (2,0)
p4 = (2,2)
pp = [p1,p2,p3,p4]
main = getLine >> getContents >>=
  print . calc . map readTuple . lines
  where readTuple = (\[x,y] -> (x,y)) . map (read :: String->Int) . words
-}
-- Counting the Ways
import Data.List
import qualified Data.IntMap.Strict as M

type MyMap = M.IntMap Int
toKey :: Int -> Int -> Int
toKey n k = (n - 1) + k * 10
bigNum :: Int
bigNum = 10^9+7
rr :: Int -> Int
rr n = n `rem` bigNum
calc :: MyMap -> Int -> [Int] -> Int -> (Int, MyMap)
calc m _ [] _ = (0,m)
calc m n (a:as) k
  | a > k     = (next , m')
  | a == k    = (rr $ 1 + next, m')
  | otherwise = let (next', m'') = getNext n (a:as) m' (k-a)
                in (rr $ next + next', m'')
  where getNext n as m k = case M.lookup (toKey n k) m of
          Nothing -> let (v,m') = calc m n as k
                     in (v, M.insert (toKey n k) v m')
          Just v  -> (v, m)
        (next,m') = getNext (n-1) as m k

main = do
  getLine
  as <- rl
  [l,r] <- rl
  print . rr . fst $ go (reverse . sort $ as) (l,r)
--  print $ go' (reverse . sort $ as) (l,r)
  where rl = fmap (map (read :: String->Int) . words) getLine
        go as (l,r) = foldl' gogo (0,M.empty) [l..r]
          where gogo (acc,m) x  = let (v,m') = calc m (length as) as x
                                  in (acc+v,m')
