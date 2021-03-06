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
-- New Year Chaos 2nd attempt
{-
import Data.List
import Data.Maybe
import qualified Data.IntMap.Strict as M
import qualified Data.ByteString.Char8 as B

bubbleSort :: [Int] -> (Maybe [Int], M.IntMap Int, Int)
bubbleSort x = foldl' bubbleSortIter (Just x, M.empty, 0) [l,l-1..1]
  where l = length x

bubbleSortIter :: (Maybe [Int], M.IntMap Int, Int) -> Int -> (Maybe [Int], M.IntMap Int, Int)
bubbleSortIter nothing@(Nothing, _, _) _ = nothing
bubbleSortIter (Just l, m, cnt) _        =  (reverse <$>l',m',cnt+cnt')
  where go (Nothing,m,c) el = (Nothing,m,c)
        go (Just [],m,c) el = (Just [el],m,c)
        go (Just f@(x:xs),m,c) el
          | el >= x                = (Just (el:x:xs), m,c)
          | M.lookup x m == Just 2 = (Nothing, m,0)
          | el <= x                 = (Just (x:el:xs),M.insertWith (+) x 1 m,c+1)
        (l',m',cnt') = foldl' go (Just [],m,0) l

check :: (Maybe [Int], M.IntMap Int,Int) -> String
check (Nothing,_,_) = "Too chaotic"
check (Just _,_,cnt) = show cnt
main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . check . bubbleSort). chunk . B.lines
  where chunk [] = []
        chunk (x:y:xs) = map (fst.fromJust.B.readInt)  (B.words y) : chunk xs
-}
{-
-- Absolute Permutation
absPerm :: [Int] -> [Int]
absPerm [n,k]
  | k == 0 = [1..n]
  | odd n  = [-1]
  | k == 1 = concatMap (\x -> [x+1,x]) [1,3..n]
  | not (isMultiple (2*k) n) = [-1]
  | otherwise = concatMap (\x -> [k+x..2*k+x-1] ++ [x..k+x-1])[1,2*k+1..n]
  where isMultiple k n = if (n `rem` k == 0) then True else False
main = getLine >> getContents >>=
  mapM_ (putStrLn . unwords . map show. absPerm) . map (map (read :: String->Int).words) . lines
-}
-- New Year Chaos 3rd attempt
-- minimum number of bribe for each node is the
-- total count of following node smaller than it
import qualified Data.IntMap.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe

countLT :: M.IntMap Int -> Int -> Int -> Int -> Maybe Int
countLT m k n acc
  | n < 0     = Nothing
  | otherwise = case M.lookupLT k m of
  Nothing -> Just acc
  Just (k',_) -> countLT m k' (n-1) (acc+1)

calc :: [Int] -> Maybe Int
calc = fst . foldr go (Just 0, M.empty)
  where go x (Nothing,_) = (Nothing, M.empty)
        go x (Just c, m)
          | r == Nothing = (Nothing, M.empty)
          | Just c' <- r = (Just (c+c'), M.insert x 1 m)
          where r = countLT m x 2 0
  
main = B.getLine >> B.getContents >>=
  mapM_ (putStrLn . check . calc). chunk . B.lines
  where chunk [] = []
        chunk (x:y:xs) = map (fst.fromJust.B.readInt)  (B.words y) : chunk xs
        check :: Maybe Int -> String
        check Nothing = "Too chaotic"
        check (Just cnt) = show cnt
