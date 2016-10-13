-- Bon Appetit
{-
import Data.Maybe
import qualified Data.ByteString.Char8 as B

calc :: Int -> Int -> [Int] -> Int -> String
calc n k l b
  | b == b' = "Bon Appetit"
  | otherwise = show $ b - b'
  where s = sum l
        b' = (s - (l !! k)) `div` 2

main = do
  [n,k] <- rl
  l <- rl
  [b] <- rl
  putStrLn $ calc n k l b
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
-}
-- Combination Lock
{-
import Data.Maybe
import qualified Data.ByteString.Char8 as B
main = do
  src <- rl
  dst <- rl
  putStrLn . show . sum $ zipWith diff src dst
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        diff :: Int -> Int -> Int
        diff a b = let v = abs (a-b) in min v (10-v)
-}
-- Flipping the Matrix
{-
import Data.Maybe
import qualified Data.ByteString.Char8 as B
calc :: (Int,[[Int]]) -> Int
calc (n,m) = sum (map sum m'')
  where m' = zipWith (\a b -> take n $ takeMax a b) m (map reverse m)
        m'' = take n $ zipWith takeMax m' (reverse m')
        takeMax a b = zipWith max a b
main = do
  [q] <- rl
  mapM_ (\_ -> putStrLn . show . calc=<< go) [1..q]
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        go = do
          [n] <- rl
          fmap (\x -> (n,x)) $ mapM (\_ -> rl) [1..2*n]
-}
-- Abbrev
{-
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Char8 as B
import Debug.Trace
calc :: String -> (String, String) -> Bool -- (Src, Dst)
calc _ ([], []) = True
calc matched ([], y:ys)
  | isUpper y      = case revMatch matched y of
                       ([],_)       -> False
                       (matched',z) -> calc matched' (reverse z, ys)
  | otherwise      = calc matched ([], ys)
calc _ (_,[]) = False
calc matched (x:xs,y:ys) -- (ABC, aAbcd)
  | x == y         = calc (y:matched) (xs, ys) -- (ABC, Abc)
  | x == toUpper y = calc (y:matched) (xs, ys) -- (ABC, abc)
  | isUpper y      = case revMatch matched y of
                       ([],_)       -> False    -- [ba] "D.." "C.."
                       (matched',z) -> calc matched' (reverse z ++ x:xs, ys)
  | otherwise      = calc matched (x:xs, ys)
revMatch :: String -> Char -> (String, String)
revMatch [] _ = ([], [])
revMatch (x:xs) y
  | isUpper x      = ([], [])
  | toUpper x == y = (y:xs, [])
  | otherwise      = let (a,b) = revMatch xs y in (a, toUpper x:b)
foo = calc [] ("ABC","aAbcd")
foo2 = calc [] ("ABC","abdABzC")
main = do
  [q] <- rl
  mapM_ (\_ -> putStrLn . (\x -> if x then "YES" else "NO") . calc [] =<< go) [1..q]
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        go = do
          a <- B.getLine
          b <- B.getLine
          return (B.unpack $ b, B.unpack $ a)
-}
-- Bonetrousle
{-
import Data.Maybe
import Data.Ratio
import qualified Data.ByteString.Char8 as B
s :: Integer -> Integer -> Integer
s a n = (a-1)*n + n*(n+1)`div`2
calc :: [Integer] -> [Integer]
-- b: number of entry, 1~k, n = summation
calc [n,k,b]
  | s 1 b > n    = [-1]
  | s (k-b+1) b < n = [-1]
  | last res > k = [-1]
  | otherwise    = res
  where a = floor $ n % b - (b+1) % 2 + 1
        d = n - s a b
        res = [a,a+1..a+b-1-d] ++ [a+b-d+1..a+b]
main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . unwords . map show . calc . readLine) . B.lines
  where readLine = map (fromIntegral . fst . fromJust . B.readInt) . B.words
-}
-- Beautiful 3 Set
calc :: Int -> (Int, [[Int]])
calc 1 = (1, [[0,0,1]])
calc 2 = (2, [[0,0,2], [1,1,0]])
calc 3 = (3, [[0,1,2], [1,2,0], [2,0,1]])
calc 4 = (3, [[0,1,3], [3,0,1], [1,3,0]])
calc 5 = (4, [[0,1,4], [1,2,2], [2,3,0],[4,0,1]])
calc 6 = (5, [[0,4,2], [1,2,3], [2,0,4], [3,3,0], [4,1,1]])
calc 7 = (5, [[0,1,6], [1,2,4], [2,3,2],[3,4,0],[4,0,3]])
--calc 8 = (5, [[0,1,7], [1,2,5], [2,3,3],[3,4,1],[4,0,4]])
calc 8 = (6, [[0,2,6], [1,5,2], [2,3,3],[3,0,5],[4,4,0],[6,1,1]])
--calc 9 = (6, [[0,1,8], [1,2,6], [2,3,4],[3,4,2],[4,5,0],[5,0,4]])
calc 9 = (7, [[0,5,4], [1,6,2], [2,0,7],[3,1,5],[4,4,1],[5,2,3],[6,3,0]])

foo n = [ [x,y,z] | x <- [0..n], y <- [0..n], let z = n-x-y, z <= n,  0<=z]
main = do
  [n] <- rl
  let (r,l) = calc n
  putStrLn $ show r
  putStr . unlines . map (unwords . map show) $ l
  where rl = fmap (map (read :: String->Int) . words) getLine
