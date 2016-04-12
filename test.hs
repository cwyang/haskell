import Data.List
import Control.Applicative

dblLinear :: Int -> Integer
dblLinear n = (twiceLinearList !! n)
iter xs = [(\x -> 2 * x + 1), (\x -> 3 * x + 1)] <*> xs
twiceLinearList = sort . concat . take 20 . iterate iter $ [1]

validBraces :: String -> Maybe String
validBraces = foldr go (Just [])
  where go x Nothing      = Nothing
        go x (Just [])    = Just [x]
        go x (Just (h:t)) = case (x,h) of
                                ('(', ')') -> Just t
                                ('[', ']') -> Just t
                                ('{', '}') -> Just t
                                otherwise  -> Nothing
{-
go :: Int -> [Int]
go n = [ b * c | c <- [1..maxc] ]
--go n = [ b * c | c <- [1..maxc], b <- [1..c-1], let a2 = 0 ]-- = ((c^2) - (b^2)), isRoot a2 ]
  where maxc = floor . sqrt n
        isRoot x = (== x) . (^2) . floor . sqrt $ x
--main = getLine >> (mapM_ (putStrLn . show . go . read) . lines =<< getContents) 
-}
conferencePicker :: [String] -> [String] -> Maybe String
conferencePicker v o = fmap head $ mapM (\x -> if elem x v then Nothing else Just x) o

f :: Integer -> [Integer]
f 0 = repeat 1
f n = scanl (+) 1 (tail . f $ n - 1)
diagonal :: Integer -> Integer -> Integer
diagonal num row = sum . take (fromInteger (num - row + 1)) . f $ row

d :: Integer -> Integer -> Integer
d num row = sum [ x * (n - x + 1) | x <- [1..n] ]
  where n = num - row + 1
