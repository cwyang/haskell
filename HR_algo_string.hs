{-
-- Sherlock and Valid String
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord

buildMap :: [Char] -> [Int] 
buildMap = map snd . M.toList . foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

decide :: [Char] -> Bool
decide s
  | same m = True
  | head m == head (tail m) + 1 && same (tail m) = True
  | last m == 1 && same (init m) = True
  | otherwise = False
  where m = sortBy (flip compare) . buildMap $ s
        same x = all (== head x) (tail x)

main = getLine >>= (putStrLn . (\x -> if x then "YES" else "NO") . decide)                            
-}
