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
-- Grid Search
import Data.List
import Data.Char

rowSearch, search :: [[Int]] -> [[Int]] -> Bool
rowSearch _ [] = True
rowSearch [] _ = False
rowSearch (row:rows) p@(pat:pats)
  | isPrefixOf pat row = rowSearch rows pats
  | otherwise          = rowSearch rows p

search [] _ = False
search ([]:_) = False
search grid pat = rowSearch grid pat || search (map tail grid) pat

go :: ([[Int]],[[Int]]) -> Bool
go (x,y) = search x y

readArgs :: [String] -> [([[Int]],[[Int]])]
readArgs [] = []
readArgs (x:xs) =
  let [col,_] = map read . words $ x
      (grid,x':xs') = splitAt col xs
      [col',_] = map read . words $ x'
      (pat,xs'') = splitAt col' xs'
      conv = map (map digitToInt)
  in (conv grid, conv pat): readArgs xs''
  
main =
  getLine >>
  getContents >>=
  (mapM_ (putStrLn . (\x -> if x then "YES" else "NO") . go) . readArgs . lines )
-- The Time in Words

numMap = undefined
  : "one"
  : "two"
  : "three"
  : "four"
  : "five"
  : "six"
  : "seven"
  : "eight"
  : "nine"
  : "ten"
  : "eleven"
  : "twelve"
  : "thirteen"
  : []
readNum :: Int -> String
readNum n
  | n < 14 = numMap !! n
  | n == 15 = "fifteen"
  | n < 20 = (numMap !! (n - 10)) ++ "teen"
  | n == 20 = "twenty"
  | n < 30 = "twenty " ++ (numMap !! (n-20))
  | n == 30 = "half"

readTime :: Int -> Int -> String
readTime hour min
  | min == 0  = readNum hour ++ " o'clock"
  | min <= 30 = readNum min ++ " past " ++ readNum hour
  | otherwise = readNum (60-min) ++ " to " ++ readNum (hour + 1)
-- Larry's Array
-- numbers are distinct. ABC BCA CAB
import Data.List
rotSort [x,y,z] = case (x < y, x < z, y < z) of
  (_, True, True) -> [x,y,z]
  (True, _, False) -> [z,x,y]
  otherwise        -> [y,z,x]

go :: [Int] -> Bool
go t@[x,y,z] = rotSort t == sort t
go x = let x' = go' x in
  go (init x')

go' :: [Int] -> [Int]
go' t@[x,y,z] = rotSort t
go' (x:y:z:xs) = let [a,b,c] = rotSort [x,y,z] in
  a: go' (b:c:xs)
  
f :: String -> [Int]
f = map read . words
test = go . f $ "9 6 8 12 3 7 1 11 10 2 5 4"
test2 = go. f $ "17 21 2 1 16 9 12 11 6 18 20 7 14 8 19 10 3 4 13 5 15"
test3 = go . f $ "5 8 13 3 10 4 12 1 2 7 14 6 15 11 9" -- this should return False
test4 = go . f $ "8 10 6 11 7 1 9 12 3 5 13 4 2"
test5 = go . f $ "7 9 15 8 10 16 6 14 5 13 17 12 3 11 4 1 18 2"
-- Matrix Rotation
type Matrix = [[Int]]
foo,bar,baz :: Matrix
foo = [[1,2],[3,4]]
bar = [[1,2,4,5],[7,8,9,10]]
baz = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
matFold :: Matrix -> ([Int], Matrix)
matFold m = let top = head m
                bottom = last m
                m' = (tail . init) m
                left = map head m'
                right = map last m'
                m'' = map (tail . init) m'
            in (top ++ right ++ reverse bottom ++ reverse left, m'')

matUnfold :: ([Int], Matrix) -> Matrix
matUnfold (l,m) = let row = length m -- we need to generate (col x (row+2) matrix)
                      col = (length l - row * 2) `div` 2
                      (top,l') = splitAt col l
                      (right,l'') = splitAt row l'
                      (rBottom,l''') = splitAt col l''
                      left = reverse l'''
                  in (top:zipWith3 (\l c r -> [l] ++ c ++ [r]) left m right) ++ [reverse rBottom]

matRotate :: Int -> Matrix -> Matrix
matRotate _ m
  | length m == 0 || length (head m) == 0 = m
matRotate n m = let (l,m') = matFold m
                    l' = rotatel n l
                    m'' = matRotate n m'
                in matUnfold(l',m'')

rotatel :: Int -> [a] -> [a]
rotatel n l = let len = n `rem` length l
                  (h,t) = splitAt len l
              in t ++ h

main = do
  header <- getLine
  content <- getContents
  let [_,_,n] = map read . words $ header
      m = map (map read . words) . lines $ content
      m' = matRotate n m
  putStrLn . unlines . map (unwords . map show) $ m'
-}
-- New Year Chaos
import Data.List

step :: Ord a => Int -> [a] -> [a] -> (Int,[a])
step n rs [] = (n, reverse (rs))
step n rs [x] = (n, reverse (x:rs))
step n rs (x:y:xs)
  | x > y = step (n+1) (x:y:rs) (xs)
  | otherwise = step n (x:rs) (y:xs)

solve :: Ord a => [a] -> (Int, [a])
solve xs = let (n, xs') = step 0 [] xs
               (n',xs'') = step n [] xs' in
           (n', xs'')

go :: [Int] -> String
go x = let (n, xs) = solve x
       in if xs == sort xs then show n
          else "Too chaotic"
               
main = getLine >> getContents >>=
  mapM_ (putStrLn . go ). chunk . lines
  where chunk [] = []
        chunk (x:y:xs) = map read  (words y) : chunk xs
{-
5 1 2 3 7 8 6 4 => Too
1 2 5 3 7 8 6 4 => 7
-}
