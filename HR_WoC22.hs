-- Matching Sets
{-
import Data.List hiding (null)
import Data.Ord
calc l l'
  | null l'                        = 0
  | head l' /= 0                   = -1
  | null (filter (>0) l') == False = -1
  | otherwise                      = sum $ filter (>0) l

removeCommon xs [] = (xs, [])
removeCommon [] xs = ([], xs)
removeCommon (x:xs) (y:ys)
  | fst x == fst y = removeCommon xs ys
  | fst x < fst y  = let (r1,r2) = removeCommon xs (y:ys) in (x:r1, r2)
  | fst x > fst y  = let (r1,r2) = removeCommon (x:xs) ys in (r1, y:r2)

reorder x y = (map fst $ sortBy (comparing snd) xx,
               map snd $ sort $ zip pos $ map fst yy)
  where
    x' = sort $ zip x [1..]
    y' = sort $ zip y y
    (xx,yy) = removeCommon x' y'
    pos = map snd xx
        
main = do
  rl
  x <- rl
  y <- rl
  let (xx, yy) = reorder x y
      z = zipWith (-) xx yy
      z' = scanr1 (+) z
  print $ calc z z'
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- Mathing Set try #2
{-
import qualified Data.IntMap.Strict as M
import Debug.Trace
calc :: M.IntMap Int -> [Int] -> Int -> Maybe Integermain = do
    rl
      x <- rl
        y <- rl
          let (xx,yy) = reorder x y
                    m = M.fromListWith (+) $ zip yy [1,1..]
                          z = calc m xx 0
                      case z of
                            Nothing -> print (-1)
                                Just v  -> print v
  where rl = fmap (map (read :: String->Int) . words) getLine

calc m [] 0 = Just 0
calc m [] _ = Nothing
calc m (x:xs) r
  | trace ("(" ++ show m ++ ")   " ++ show (x:xs) ++ " / " ++ show r ) False = undefined
  | Just 1 <- eqX = calc (mdel x) xs r
  | Just _ <- eqX = calc (mdec x) xs r
  | Just (k,1) <- ltX = fmap (+(x-k)) $ calc (mdel k) xs (r+x-k)
  | Just (k,_) <- ltX = fmap (+(x-k)) $ calc (mdec k) xs (r+x-k)
  | Just (k,1) <- gtX = if r >= k-x then calc (mdel k) xs (r-k+x) else Nothing
  | Just (k,_) <- gtX = if r >= k-x then calc (mdec k) xs (r-k+x) else Nothing
  | otherwise = Nothing
  where eqX = M.lookup x m
        ltX = M.lookupLT x m
        gtX = M.lookupGT x m
        mdel = flip M.delete m
        mdec = flip (M.adjust pred) m
foo1 = calc (M.fromListWith (+) $ zip [100,3,3] [1,1..]) [3,1,1] 0
main = do
  rl
  x <- rl
  y <- rl
  let m = M.fromListWith (+) $ zip y [1,1..]
      z = calc m x 0
  case z of
    Nothing -> print (-1)
    Just v  -> print v
  where rl = fmap (map (read :: String->Int) . words) getLine
-}
-- Matching Set try #3
{-
import Data.List hiding (null)
import Data.Ord
import qualified Data.ByteString.Char8 as B
import Data.Maybe
calc l lsum
  | lsum /= 0 = -1
  | otherwise = sum $ filter (>0) l
main = do
  rl
  x <- fmap sort $ rl
  y <- fmap sort $ rl
  let z = zipWith (-) x y
      z' = foldl1' (+) z
  print $ calc z z'
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
-}
-- 101 -> 000, 100, 001, 101
{-
-- Submask Queries
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.IntMap.Strict as M
import Data.Bits
import Data.Char
import Debug.Trace
bitExpand' :: [Int] -> [[Int]]
bitExpand' [] = [[]]
bitExpand' (x:xs)
  | x == 0    = map (0:) next
  | otherwise = map (0:) next ++ map(1:) next
  where next = bitExpand' xs

bitExpand :: [Int] -> [Int]
bitExpand = map toNum . bitExpand'

toNum :: [Int] -> Int
toNum [] = 0
toNum (x:xs) = x + (toNum xs * 2)
toList :: B.ByteString -> [Int]
toList = map digitToInt . B.unpack
        
updateIntMap :: M.IntMap Int -> [(Int, Int)] -> M.IntMap Int
updateIntMap m [] = m
updateIntMap m ((k,v):xs) = updateIntMap m' xs
  where m' = M.insert k v m

xorIntMap :: M.IntMap Int -> [(Int, Int)] -> M.IntMap Int
xorIntMap m [] = m
xorIntMap m ((k,v):xs) = xorIntMap m' xs
  where  m' = M.adjust (\x -> x `xor` v) k m
         
calc :: M.IntMap Int -> [B.ByteString] -> IO (M.IntMap Int)
calc m (cmd:arg:arg2)
--  | trace (show cmd ++ " " ++ show arg) False = undefined
  | otherwise = do
  case cmd of
    "1" -> return . updateIntMap m $ zip args (repeat (toInt arg))
    "2" -> return . xorIntMap m $ zip args (repeat (toInt arg))
    "3" -> do
      putStrLn . show . fromJust $ M.lookup argNum m
      return m
  where args = bitExpand (toList $ head arg2)
        argNum = toNum (toList arg)
        toInt = fst . fromJust . B.readInt
  
main :: IO ()
main = do
  [n,m] <- rl
  loop M.empty m
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        loop :: M.IntMap Int -> Int -> IO ()
        loop _ 0 = return ()
        loop m cnt = do
          cmd <- fmap (B.words) B.getLine
          m' <- calc m cmd
          loop m' (cnt-1)
--3 4
--1 3 110
--3 100
-}
{-
-- Submask Queries 2nd attempt
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import Data.Char
import Data.Bits
import qualified Data.ByteString.Char8 as B
data Cmd
  = Ins Int Mask
  | Xor Int Mask
  | Lookup Mask
type Mask = Int

toNum :: [Int] -> Int
toNum [] = 0
toNum (x:xs) = x + (toNum xs * 2)
toList :: B.ByteString -> [Int]
toList = map digitToInt . B.unpack

match var mask = complement (mask .|. complement var) == 0
calc :: [Cmd] -> Int -> Int
calc [] mask = 0
calc ((Ins x mask'):cmds) mask
  | match mask mask' = x
calc ((Xor x mask'):cmds) mask 
  | match mask mask' = x `xor` calc cmds mask
calc (_:cmds) mask = calc cmds mask
parse :: [B.ByteString] -> Cmd
parse (cmd:x:s)
  | cmd == "1" = Ins (toInt x) (toNum . toList . head $ s)
  | cmd == "2" = Xor (toInt x) (toNum . toList . head $ s)
  | otherwise  = Lookup $ toNum . toList $ x
  where toInt = fst . fromJust . B.readInt
main :: IO ()
main = do
  [n,m] <- rl
  let foo = m
  loop [] m
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        loop :: [Cmd]  -> Int -> IO ()
        loop _ 0 = return ()
        loop cmds cnt = do
          cmd <- fmap (B.words) B.getLine
          let pcmd = parse cmd
          case pcmd of
            Ins x mask -> loop (pcmd:cmds) (cnt-1)
            Xor x mask -> loop (pcmd:cmds) (cnt-1)
            Lookup mask -> do
              putStrLn . show $ calc cmds mask
              loop cmds (cnt-1)
-}
{-
-- Number of Sequences - Too Hard!
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Array
import Data.List

factorize :: Integer -> Integer -> [Integer]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

primeFactors :: Integer -> [Integer]
primeFactors = nub . factorize 2

calc :: Array Int Int -> Int -> Int -> Integer
calc arr n idx
  | idx > n         = 1
  | arr ! idx /= -1 = calc arr n (idx+1)
  | any (/= (-1)) [ arr ! x | x <- [idx, idx*2..n] ] = calc arr n (idx + 1)
  | otherwise = fromIntegral idx * calc arr n (idx+1)
  where vs = reverse [ (x, arr ! x) | x <- [idx, idx*2..n]]
        v = find (\(_,val) -> val == -1) vs
main = do
  [n] <- rl
  l <- rl
  print $ calc (listArray (1,n) l) n 2 `rem` (10^9+7)
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
-}
-- Sequential Prefix Function
{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Control.Monad (liftM)
data Cmd
  = Ins {val::Int}
  | Del
parse :: [B.ByteString] -> Cmd
parse (cmd:x)
  | cmd == "+" = Ins (toInt . head $ x)
  | cmd == "-" = Del
  where toInt = fst . fromJust . B.readInt
calc :: [Int] -> IntTrie -> Int
calc [] _ = 0
calc l m = findLen (init l) m
{-calc l m = length . fromJust . head . dropWhile (== Nothing) . map (\x -> M.lookup x m) $ p
  where p = tail . reverse $ inits l
-}
{-
calc l m = length . fst . fromJust $ find (\(x,y) -> x == y) p
  where p = tail $ zip (reverse $ inits l) (tails l)
-}
        
main :: IO ()
main = do
  [n] <- rl
  loop [] emptyTrie n
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        loop :: [Int] -> IntTrie -> Int -> IO ()
        loop _ _ 0 = return ()
        loop l m cnt = do
          cmd <- fmap (B.words) B.getLine
          let pcmd = parse cmd
          case pcmd of
            Ins x -> do
              let l' = (x:l)
                  m' = insertTrie 0 l' m
              putStrLn . show $ calc l' m'
              loop l' m' (cnt - 1)
            Del   -> do
              let l' = tail l
                  m' = removeTrie l m
              putStrLn . show $ calc l' m'
              loop l' m' (cnt - 1)
-------------------
data IntTrie = Trie { value :: (Int,Bool), children :: M.IntMap IntTrie } deriving (Show)
emptyTrie :: IntTrie
emptyTrie = Trie { value = (0, False), children = M.empty}
insertTrie :: Int -> [Int] -> IntTrie -> IntTrie
insertTrie lvl [] t     = t { value = (lvl,True) }
insertTrie lvl (k:ks) t = let ts = children t
                      in case M.lookup k ts of
                         Nothing -> t { children = M.insert k (insertTrie (lvl+1) ks emptyTrie) ts }
                         Just t' -> t { children = M.insert k (insertTrie (lvl+1) ks t') ts }
removeTrie :: [Int] -> IntTrie -> IntTrie
removeTrie [] t     = t { value = (0,False) }
removeTrie (k:ks) t = let ts = children t
                      in case M.lookup k ts of
                         Nothing -> error "should not happen"
                         Just t' -> t { children = M.insert k (removeTrie ks t') ts }
findLen :: [Int] -> IntTrie -> Int
findLen [] t
  | valueNode = v
  | otherwise = 0
  where (v,valueNode) = value t
findLen (k:ks) t
  | valueNode == True = case M.lookup k (children t) of
                           Nothing -> v
                           Just t' -> max v (findLen ks t')
  | otherwise  = case M.lookup k (children t) of
                           Nothing -> 0
                           Just t' -> findLen ks t'
  where (v,valueNode) = value t

foo = i [1,2,4] $ i [1,2,3] $ i [2,1,3] $ emptyTrie
  where i = insertTrie 0
bar = i [1] $ emptyTrie
  where i = insertTrie 0


f1,f2 :: [Int] -> Int
f1 [] = 0
f1 l = findLen l m
  where m = foldl' go emptyTrie $ tail . tails $ l
        go trie x = insertTrie 0 x trie
f2 [] = 0
f2 l = length . fst . fromJust $ find (\(x,y) -> x == y) p
  where p = tail $ zip (reverse $ inits l) (tails l)
test1 :: [Int] -> Bool
test1 l = f1 l == f2 l
fooTest = do
  quickCheck (test1)
