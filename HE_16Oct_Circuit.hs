-- set of one subst (over length 2) reversed
{-
import Data.List

revOne :: [a] -> [[a]]
revOne xs = [ as++reverse bs++cs | (as,bs') <- zip (inits xs) (tails xs),
                        (bs,cs)  <- tail $ zip (inits bs') (tails bs'),
                        length bs > 1]
revTwo :: [a] -> [[a]]
revTwo xs = concat [ map ((as++reverse bs)++) $ revOne cs | (as,bs') <- zip (inits xs) (tails xs),
                        (bs,cs)  <- tail $ zip (inits bs') (tails bs'),
                        length bs > 1, length cs > 1]

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

isAnswer :: [Char] -> Bool
isAnswer xs
  | isPal xs = True
  | any isPal (revOne xs) = True
  | any isPal (revTwo xs) = True
  | otherwise = False
    
main :: IO ()
main = getLine >> getContents >>=
  print . length . filter id . map isAnswer . lines
-}
{-
import Data.Array

rnd :: Int -> Int
rnd x = (x * 37 + 10007) `mod` 1000000007

sign :: Int -> (Int, Int)
sign x
  | x' > 500000003 = (1, x')
  | otherwise      = (-1, x')
  where x' = rnd x

rng :: Int -> (Int, Int)
rng seed = (s*value , seed')
  where value = rnd seed;
        (s,seed') = sign value;

mkArray :: Int -> Int -> Array (Int,Int,Int) Int
mkArray seed n = accumArray (+) 0 ((1,1,0),(n,n,c)) $ genList seed 1 2 0
  where c = floor (f (n* n) / 4)
        f = fromIntegral
        genList seed i j k
          | i > n = []
          | j > n = genList seed (i+1) (i+2) 0
          | k > c = genList seed i (j+1) 0
          | otherwise = let (v,seed') = rng seed
                        in ((i,j,k),v):genList seed' i j (k+1)
--main :: IO ()
--main = getLine >> getContents >>=  print . length . filter id . map isAnswer . lines
-}
{-
import Data.List
import Debug.Trace

calc :: [Int] -> [Int] -> [Int] -> Int
calc _ _ [] = 0
calc acc [] (x:xs) = calc [] (reverse acc) xs
calc acc (m:ms) (x:xs)
--  | trace (show (acc, (m:ms), (x:xs))) False = undefined
  | x `rem` m == 0 = 1 + calc [] (reverse acc ++ ms) xs
  | otherwise      = calc (m:acc) ms (x:xs)

main= do
  [n,m] <- fmap (map (read :: String -> Int) . words) getLine
  l <- fmap (map (read :: String -> Int) . lines) getContents
  print $ calc [] [m,m-1..1] $ reverse $ sort l
                
foo = calc [] [8,7..1] [16,2,4,1,32] -- 4,1,2,3,8
-}
