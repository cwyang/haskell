{-
-- Encryption
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l = let (u,v) = splitAt n l in u:chunks n v

transpose:: [[a]] -> [[a]]
transpose [xs]     = [[x] | x <- xs]
transpose (xs:xss) = myZipWith (:) xs (transpose xss)
  where
    myZipWith _ [] [] = []
    myZipWith _ xs [] = [[x] | x <- xs]
    myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

gridify :: String -> [String]
gridify s = let len = length s
                col = ceiling(sqrt (fromIntegral len))
                row = if (col * (col-1) >= len) then col-1
                      else col
            in (chunks col s)
-- Cavity Map
import Data.Char
import Data.List

mkDigitList :: [String] -> [[Int]]
mkDigitList = map (map digitToInt)

mkStringList :: [[Int]] -> [String]
mkStringList = map (map (\x -> if x == 0 then 'X' else intToDigit x))

leftAdj, rightAdj, topAdj, bottomAdj :: [[Int]]-> [[Int]]
leftAdj = map (10:)
rightAdj = map (tail . (++ [10]))
topAdj = (repeat 10 :)
bottomAdj = tail . (++ [repeat 10])

mkCavityList :: [[Int]] -> [[Int]]
mkCavityList lst = zipWith5 (zipWith5 go) lst l r t b
  where l = leftAdj lst
        r = rightAdj lst
        t = topAdj lst
        b = bottomAdj lst
        go u v w x y | u>v && u>w && u>x && u>y = 0
                     | otherwise = u
main = getLine >> getContents >>= (putStrLn . unlines . mkStringList . mkCavityList . mkDigitList . lines)
-}
