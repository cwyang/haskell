--A
{-
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf

calc :: [Int] -> String
calc [_] = "-1"
calc l
  | v == 15 = "DOWN"
  | v == 0  = "UP"
  | v > w   = "UP"
  | otherwise = "DOWN"
  where v = last l
        w = last $ init l
main = do
  [n] <- rl
  l <- rl
  putStrLn . calc $ l
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
-}
--
--B.
{-
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe

calc :: String -> Int
calc xs = min ynum znum
  where l = length xs
        ys = take l $ cycle "rb"
        zs = take l $ cycle "br"
        ynum = diff xs ys
        znum = diff xs zs

diff :: String -> String -> Int
diff xs ys = max r b
  where zs = zip xs ys
        ds = map fst $ filter (\(x,y) -> x /= y) zs
        (rs,bs) = partition (=='r') ds
        r = length rs
        b = length bs
main = do
  getLine
  l <- getLine
  putStrLn . show . calc $ l
-}
-- C
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as M
import Data.List
import Data.Maybe
import Data.Char

calc :: Int -> String -> String
calc 0 xs = xs
calc t xs = calc' t [] xs

calc', calc2 :: Int -> String -> String -> String
calc' _ acc [] = reverse acc  -- no digit
calc' 0 acc _ = reverse acc
calc' t acc ('.':xs) = calc2 t ('.':acc) xs
calc' t acc (x:xs) = calc' t (x:acc) xs

calc2 _ acc [] = reverse acc
calc2 t acc (x:xs)
  | x < '5' = calc2 t (x:acc) xs
  | otherwise = calc3 (t-1) acc

calc3 :: Int -> String -> String
calc3 0 ('.':xs)

main = do
  [n,t] <- rl
  l <- getLine
  putStrLn . calc t $ l
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
