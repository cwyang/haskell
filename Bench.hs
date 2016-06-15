module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as SEQ
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector ((//))
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

infixl 9 !?
infixl 9 !@

_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

{-# INLINABLE (!@) #-}
(!@) :: [a] -> Int -> Maybe a
xs !@ n
  | n < 0 = Nothing
  | otherwise =
      foldr (\x r k -> case k of
                0 -> Just x
                _ -> r (k-1)) (const Nothing) xs n
      
myList :: [Int]
myList = [1..9999]

main2 :: IO ()
main2 = defaultMain
  [
    bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998
  , bench "index list  improved"
    $ whnf (myList !@) 9998
  ]

myList2 :: [Int]
myList2 = undefined:[1..9999]
myList3 :: [Int]
myList3 = (undefined : undefined)
myList4 :: [Int]
myList4 = undefined
main3 :: IO ()
main3 = defaultMain
  [ bench "map list 9999" $ nf (map (+1)) myList
  ]

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0 xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)
pairList :: [(String, Int)]
pairList = genList 9001
testMap :: M.Map String Int
testMap = M.fromList pairList

main4 :: IO ()
main4 = defaultMain
  [
    bench "lookup list" $
    whnf (lookup "doesntExist") pairList
  , bench "lookup map" $
    whnf (M.lookup "doesntExist") testMap
  ]

bumpIt (i,v) = (i+1,v+1)
m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0,0)
s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0
membersMap, membersSet :: Int -> Bool
membersMap = flip M.member m
membersSet = flip S.member s

main5 :: IO ()
main5 = defaultMain
  [ bench "member map" $
    whnf membersMap 9999
  , bench "member set" $
    whnf membersSet 9999
  ]

lists :: [[Int]]
lists = replicate 10 [1..100000]
seqs :: [SEQ.Seq Int]
seqs = replicate 10 (SEQ.fromList [1..100000])

l22 :: [Int]
l22 = [1..100000]
s22 :: SEQ.Seq Int
s22 = SEQ.fromList [1..100000]
main6 :: IO ()
main6 = defaultMain
  [ bench "concat list" $
    nf mconcat lists
  , bench "concat seq" $
    nf mconcat seqs
  , bench "index list" $
    whnf (\xs -> xs !! 9001) l22
  , bench "index seq" $
    whnf (flip SEQ.index 9001) s22
  ]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) $ drop from xs
l33 :: [Int]
l33 = [1..10000]
v :: V.Vector Int
v = V.fromList l33
main7 :: IO ()
main7 = defaultMain
  [ bench "slicing list" $
    whnf (head . slice 1000 9000) l33
  , bench "slicing vector" $
    whnf (V.head . V.slice 1000 9000) v
  ]

testV' :: Int -> V.Vector Int
testV' n =
  V.map (+n) $ V.map (+n) $
    V.map (+n) $ V.map (+n)
    (V.fromList [1..10000])

testV :: Int -> V.Vector Int
testV n =
  V.map ( (+n) . (+n) . (+n) . (+n) )
    (V.fromList [1..10000])
main8 :: IO ()
main8 = defaultMain
  [ bench "vector map prefuse" $
    whnf testV 9998
  , bench "vector map fuse" $
    whnf testV' 9998
  ]

vec :: V.Vector Int
vec = V.fromList [1..10000]
slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n-1) (v // [(n,0)])
batch :: Int -> V.Vector Int
batch n = vec // updates
  where updates = map (\n -> (n,0)) [0..n]
batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
  where updates = fmap (\n -> (n, 0)) (V.fromList [0..n])
mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n-1) v
mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = V.freeze v
        go n v = (MV.write v n 0) >> go (n -1) v
main9 :: IO ()
main9 = defaultMain
  [ bench "slow" $ whnf slow 9998
  , bench "batch" $ whnf batch 9998
  , bench "batchVector" $ whnf batchVector 9998
  , bench "mutable IO vector" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $ whnf mutableUpdateST 9998
  ]

{-
--regress allocated:iters +RTS -T
Useful regressions
  regression--regressnotes
  CPU cyclescycles:iters
  Bytes allocatedallocated:iters+RTS -T
  Number of garbage collectionsnumGcs:iters+RTS -T
  CPU frequencycycles:time
-}

a :: [Int]
a = [1..100000]
boxV :: [Int] -> V.Vector Int
boxV v = V.fromList v
unboxV :: [Int] -> VU.Vector Int
unboxV v =VU.fromList $ v
main :: IO ()
main = (a!!99999) `seq` defaultMain
  [ bench "box" $ nf boxV a
  , bench "unbox" $ nf unboxV a
  ]
