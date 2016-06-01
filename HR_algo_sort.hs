{-
-- Insertion sort procession
import Data.List

insSort :: Ord a => [a] -> [[a]]
insSort l = insSort' [] l

insSort' :: Ord a => [a] -> [a] -> [[a]]
insSort' sorted [] = [sorted]
insSort' sorted p@(x:xs) = (sorted++p):insSort' sorted' xs
  where (a,b) = partition (< x) sorted
        sorted' = a ++ (x:b)

main = getLine >> getContents >>=
      (putStrLn . unlines . map (unwords . map show) . tail . tail . insSort . map read . words)
-- Counting Sort
import Data.Array
import Data.List

go :: [Int] -> [Int]
go = elems . foldl' update (array (0,99) [(i,0) | i <- [0..99]])
  where update acc x = let acc' = acc // [(x,acc ! x + 1)]
                       in elems acc' `seq` acc' -- deepseq
main = getLine >> getLine >>= (putStrLn . unwords . map show . go . map read . words)
-- Counting Sort4
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe

filt :: Int -> [(Int, B.ByteString)] -> [(Int,B.ByteString)]
filt 0 x = x
filt k ((n,s):xs) = (n,B.pack "-"): filt (k-1) xs

go :: [(Int,B.ByteString)] -> [B.ByteString]
go l= map (B.unwords . reverse) $ M.elems arr
  where update acc (x,s) = M.insertWith (++) x [s] acc
        arr = foldl' update initM l
        initM = foldl' (\acc x -> M.insert x [] acc) M.empty [0..99]
        
main = do
  l1 <- B.getLine
  contents <- B.getContents
  let n = fst . fromJust . B.readInt $ l1
  B.putStrLn . B.unwords . go . filt (n `div` 2).
  map ((\[x,y] -> (fst . fromJust . B.readInt $ x,y)) . B.words). B.lines $ contents

-- Almost Sorted
import Data.List
import Data.Maybe
import Text.Printf

findInd :: Int -> (a->a->Bool) -> [a] -> Maybe (Int, a)
findInd _ _ [] = Nothing
findInd _ _ [x] = Nothing
findInd k pred (x:y:xs) =
    if pred x y then Just (k,x) else findInd (k+1) pred (y:xs)
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x > y = False
                  | otherwise = isSorted (y:xs)
go :: Int -> [Int] -> [String]
go n fl
  | fidx == Nothing    = ["yes"]
  | isSorted (take fidx' fl ++ swapHeadLast mid ++ (take 1 (drop (ridx'+1) fl))) =
      ["yes", printf "swap %d %d" (fidx'+1) (ridx'+1)]
  | isSorted (take fidx' fl ++ reverse mid ++ (take 1 (drop (ridx'+1) fl))) =
      ["yes", printf "reverse %d %d" (fidx'+1) (ridx'+1)]
  | otherwise          =  ["no"]
  where fidx = findInd 0 (>) fl
        ridx = findInd 0 (<) (reverse fl)
        (fidx', fx) = fromJust fidx
        (ridx'', rx) = fromJust ridx
        ridx' = n - ridx'' - 1
        mid = take (ridx'-fidx'+1) . drop fidx' $ fl
        swapHeadLast (x:xs) = last xs:init xs ++ [x]

test = go 100 fl
fl =map read $ words "4104 8529 49984 54956 63034 82534 84473 86411 92941 95929 108831 894947 125082 137123 137276 142534 149840 154703 174744 180537 207563 221088 223069 231982 249517 252211 255192 260283 261543 262406 270616 274600 274709 283838 289532 295589 310856 314991 322201 339198 343271 383392 385869 389367 403468 441925 444543 454300 455366 469896 478627 479055 484516 499114 512738 543943 552836 560153 578730 579688 591631 594436 606033 613146 621500 627475 631582 643754 658309 666435 667186 671190 674741 685292 702340 705383 722375 722776 726812 748441 790023 795574 797416 813164 813248 827778 839998 843708 851728 857147 860454 861956 864994 868755 116375 911042 912634 914500 920825 979477"
main = do
  l1 <- getLine
  let n = read l1
  getLine >>=
    putStrLn . unlines . go n . map read . words  
-}
-- Qsort in place by R.Bird
import Data.Array.ST
import Control.Monad.ST

qsort :: Ord a => [a] -> [a]
qsort xs = runST $ let n = length xs in 
  do {xa <- newListArray (0,n-1) xs;
      qsortST xa (0,n-1);
      getElems xa}

-- sort the elements of xa in thte interval [a,b)
{-
qsortST :: Ord a => STArray s Int a -> (Int, Int) -> ST s [[a]]
qsortST xa (a,b)
  | a >= b    = return []
  | otherwise = do m <- partition xa (a,b)
                   part <- getElems xa
                   r1 <- qsortST xa (a,m-1)
                   r2 <- qsortST  xa (m+1,b)
                   return $ part:r1 ++ r2
-}                           
qsortST :: Ord a => STArray s Int a -> (Int, Int) -> ST s ()
qsortST xa (a,b)
  | a >= b    = return ()
  | otherwise = do m <- partition xa (a,b)
                   qsortST xa (a,m-1)
                   qsortST  xa (m+1,b)
                   return ()

partition :: Ord a => STArray s Int a -> (Int,Int) -> ST s Int
partition xa (a,b)
  = do x <- readArray xa b
       let {loop (j,k) = if k == b
                         then do swap xa b j
                                 return j
                         else do y <- readArray xa k
                                 if y > x then loop (j,k+1)
                                   else do swap xa j k
                                           loop (j+1,k+1)}
       loop (a,a)

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap xa i j = do v <- readArray xa i
                 w <- readArray xa j
                 writeArray xa i w
                 writeArray xa j v
       
