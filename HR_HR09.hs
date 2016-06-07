{-
-- Fair Ration
import Data.List

countBread :: Int -> [Int] -> Int
countBread acc [] = acc
countBread acc (x:y:xs)
  | even x = countBread acc (y:xs)
  | odd x && odd y = countBread (acc+2) xs
  | otherwise      = countBread (acc+2) ((y+1):xs)
countBread acc [x]
  | even x = acc
  | odd x  = -1
main = do
  [n] <- rl
  l <- rl
  let res = countBread 0 l
  if res < 0 then putStrLn "NO" else print res
  where rl = fmap (map (read :: String->Int) . words) getLine

-- Mandragora Forest
import Data.List (sort, scanr1)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
go :: Int -> [Int] -> Int
go s l = go' s l (scanr1 (+) l)

go' :: Int -> [Int] -> [Int] -> Int
go' s l@(x:xs) sl@(y:ys)
  | v1 > v2 = v1
  | otherwise = go' (s+1) xs ys
  where v0 = y - x
        v1 = s * (v0 + x)
        v2 = (s+1) * v0

chunk :: [B.ByteString] -> [[Int]]
chunk [] = []
chunk (x:y:xs) = let l = map (fst . fromJust . B.readInt) . B.words $ y
                 in l : chunk xs
main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . show . go 1 . sort) . chunk . B.lines
-}
-- Dummy
import Data.Maybe
import qualified Data.ByteString.Char8 as B
chunk :: [B.ByteString] -> [[Int]]
chunk [] = []
chunk (x:y:xs) = let l = map (fst . fromJust . B.readInt) . B.words $ y
                 in l : chunk xs
main =  do 
  [n] <- rl
  mapM_ (\x -> B.getLine) [1..n]
  [qn] <- rl
  mapM_ (\_ -> rl >>= (\x -> putStrLn . show $ last x - 1 )) [1..qn]
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
