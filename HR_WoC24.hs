{-
-- Simplified Chess Engine
import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace

type Pos = (Int,Int)
data PType = Queen | Rook | Bishop | Knight deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
data Piece = Piece { ptype :: PType
                   , color :: Color
                   , pos   :: Pos} deriving (Show, Eq)
data Loc   = Loc { getPiece:: Piece } | Empty deriving (Show, Eq)
data Chess = Chess { board :: Array (Int,Int) Loc
                   , my  :: [Piece]
                   , opp :: [Piece]
                   , turn :: Color } deriving (Show)

moves :: Chess -> Color -> Pos -> Pos -> [Pos]
moves chess c (x,y) (dx,dy)
  | u < 0 || u > 3 || v < 0 || v > 3 = []
  | l == Empty = (u,v): moves chess c (u,v) (dx,dy)
  | color p == c = []
  | otherwise = [(u,v)]
  where (u,v) = (x+dx,y+dy)
        b = board chess
        l = b ! (u,v)
        p = getPiece l

canCapture :: Chess -> Piece -> Piece -> Bool
canCapture chess atk tgt = elem (pos tgt) $ validMoves chess atk

validMoves :: Chess -> Piece -> [Pos]
validMoves chess piece
  | typ == Queen  = concat [ moves chess c (x,y) delta | delta <- [(-1,-1),(1,1),(-1,1),(1,-1),(0,-1),(0,1),(-1,0),(1,0)] ]
  | typ == Rook   = concat [ moves chess c (x,y) delta | delta <- [(0,-1),(0,1),(-1,0),(1,0)] ]
  | typ == Bishop = concat [ moves chess c (x,y) delta | delta <- [(-1,-1),(1,1),(-1,1),(1,-1)] ]
  | otherwise     = concat [ moves chess c (x,y) delta | delta <- [(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(-2,-1),(2,1),(-2,1)] ]
  where typ = ptype piece
        (x,y) = pos piece
        c = color piece

makeMove :: Chess -> Piece -> Pos -> Chess
makeMove chess p (u,v)
  | b ! (u,v) == Empty = chess { board = b // [((u,v),Loc p'),((x,y),Empty)]
                               , my = opp chess
                               , opp = p' : (delete p $ my chess)
                               , turn = next (turn chess) }
  | otherwise          = chess { board = b // [((u,v),Loc p'),((x,y),Empty)]
                               , my = delete q $ opp chess
                               , opp = p' : (delete p $ my chess)
                               , turn = next (turn chess) }
  where (x,y) = pos p
        b = board chess
        p' = p{pos = (u,v)}
        q = getPiece $ b ! (u,v)
        next White = Black
        next _ = White

play :: Chess -> Either Color [Chess]
play chess
  | isOver    = if color oppQueen == White then Left Black else Left White
  | otherwise = Right [ makeMove chess x p  | x <- my chess, p <- validMoves chess x ]
  where oppQueen = fromJust . find ((== Queen) . ptype) $ opp chess
        isOver = any id . map (flip (canCapture chess) oppQueen) $ my chess

makePiece :: Color -> String -> Piece
makePiece clr s = Piece pt clr (x,y)
  where [a,b,c] = words s
        y = read c - 1
        x = case b of
          "A" -> 0
          "B" -> 1
          "C" -> 2
          "D" -> 3
        pt = case a of
          "Q" -> Queen
          "R" -> Rook
          "B" -> Bishop
          "N" -> Knight
    
genChess :: [String] -> [String] -> Chess
genChess w b = Chess {board = brd, my = white, opp = black, turn = White}
  where white = map (makePiece White) w
        black = map (makePiece Black) b
        brd = array ((0,0),(3,3)) $ [((x,y),Empty) | x <- [0..3], y <- [0..3] ] ++
          [ (pos p,Loc p) | p <- white ++ black ]
chunk :: [String] -> [(Int,Chess)]
chunk [] = []
chunk (x:xs) = let (wpiece,xs')  = splitAt w xs
                   (bpiece,xs'') = splitAt b xs'
               in (m, genChess wpiece bpiece) : chunk xs''
  where rl = map (read :: String->Int) . words
        [w,b,m] = rl x

calc :: Int -> Chess -> Bool
calc k chess
  | k == 0 = False
  | otherwise = case play chess of
      Left White -> True
      Left Black -> False
      Right chess' -> case turn chess of
        White -> any id $ map (calc (k-1)) chess'
        Black -> all id $ map (calc (k-1)) chess'

main :: IO ()
main = getLine >> getContents >>=
  mapM_ (putStrLn . (\x -> if x then "YES" else "NO") . uncurry calc) .  chunk .lines

a = ["2 1 1",
     "Q B 2",
     "Q B 1",
     "Q A 4"]
b = snd . head $ chunk a
foo x =  map (flip (canCapture x) (oppQueen x)) $ my x
oppQueen x = fromJust . find ((== Queen) . ptype) $ opp x
myQueen x = fromJust . find ((== Queen) . ptype) $ my x
-}
-- XOR Matrix
{-
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import Data.Bits
import System.Random
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (..))
xorBrute2 :: Int -> Int -> [Int] -> [Int]
xorBrute2 m n xs = runST $ do
  arr <- newListArray (0,n-1) xs
  go (m-1) arr
  elems <$> freeze arr
  where go :: Int -> STArray s Int Int -> ST s ()
        go 0 _   = return ()
        go m arr = do
          z <- readArray arr 0
          y <- readArray arr (n-1)
          forM_ [0..n-2] $ \x -> do
            a <- readArray arr x
            b <- readArray arr (x+1)
            writeArray arr x (xor a b)
          writeArray arr (n-1) (xor z y)
          go (m-1) arr

xorBrute, xor1,xor2 :: Int -> [Int] -> [Int]
xorBrute 0 xs = xs
xorBrute n xs = xorBrute (n-1) $ zipWith xor xs (tail xs ++ [head xs])

xor1 = xor1' Map.empty
xor1' :: Map.Map [Int] Int -> Int -> [Int] -> [Int]
xor1' _ 0 xs = xs
xor1' dict n xs
  = case Map.lookup xs dict of
  Nothing -> xor1' dict' (n-1) xs'
  Just v -> trace ("gotit! " ++ show (v-n)) $ xorBrute ((n-1) `rem` (v-n)) xs'
  where xs' = zipWith xor xs (tail xs ++ [head xs])
        dict' = Map.insert xs n dict

xor2 = xor2' Map.empty -- wrong..
xor2' :: Map.Map [Int] Int -> Int -> [Int] -> [Int]
xor2' _ 0 xs = xs
xor2' dict n xs
  = case (Map.lookup xs dict, Map.lookup (tail xs ++ [head xs]) dict) of
  (Just v,_) -> trace ("gotit! " ++ show (v-n)) $ xorBrute (f v) xs'
  (Nothing, Nothing) -> xor2' dict' (n-1) xs'
  (Nothing, Just v) -> trace ("shit! " ++ show (v-n)) $ xorBrute (f v) $ rotater (g v) xs'
  where xs' = zipWith xor xs (tail xs ++ [head xs])
        dict' = Map.insert xs n dict
        f v = mod (n-1) (v-n)
        g v = mod (div (n-1) (v-n)) n

rotatel,rotater :: Int -> [a] -> [a]
rotatel _ [] = []
rotatel n xs = zipWith const (drop n (cycle xs)) xs
rotater _ [] = []
rotater n xs = zipWith const (drop n' (cycle xs)) xs
  where n' = length xs - n
        
main = do
  [n,m] <- rl
  a <- rl
  B.putStrLn . B.unwords . map (B.pack . show) . xor1 (m-1) $ a
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine

prop_xor :: (NonEmptyList Int) -> Int -> Bool
prop_xor xs n = xorBrute n (g xs) == xor1 n (g xs)
  where g = getNonEmpty
prop_xor2 :: (NonEmptyList Int) -> Int -> Bool
prop_xor2 xs n = xor2 n (g xs) == xor1 n (g xs)
  where g = getNonEmpty
checkXor = do
  quickCheck prop_xor2

checkFoo n =
  let a = mkRandomInts n
  in any id $ flip map [10^18..10^18] (\x -> xor1 x a == xorBrute x a)
checkBar n =
  xor1 (10^18) [1..n]
checkBaz n =
  xor2 (10^18) [1..n]
mkRandomInts :: Int -> [Int]
mkRandomInts n = go n (mkStdGen 0)
  where go 0 _ = []
        go n gen = let (a,gen') = (random gen)
                   in (abs a `mod` 10^18):go (n-1) gen'

foo n = do
  let a = map (flip xorBrute $ [1..n]) [1..10]
  mapM_ (\(k,v) -> putStrLn $ show k ++ ": " ++ show v) $ zip [1..] a
bar n = do
  let a = map (flip xor1 $ [1..n]) [1..100]
  mapM_ (\(k,v) -> putStrLn $ show k ++ ": " ++ show v) $ zip [1..] a
a = map (flip xorBrute $[100]++[1..6]) [1..8]
b = map (flip xorBrute $[100]++[1..7]) [1..8]
c = map (flip xorBrute $[100]++[1..8]) [1..40
                               ]
test xs = go 1 xs' (xorBrute 1 xs')
  where xs' = xorBrute 1 xs
        go k tgt xs
          | xs == tgt = k
          | all (== 0) xs = (negate k)
          | otherwise = go (k+1) tgt $ xorBrute 1 xs
-}
-- Shashank and Palindrome
{-
import Data.List

calc :: [String] -> [String]
calc [] = [[]]
calc (x:xs) = [ y ++ z | y <- (tail $ subStrings x), z <- calc xs ]

subStrings :: String -> [String]
subStrings [] = [[]]
subStrings (x:xs) = let v = subStrings xs
                    in v ++ map (x:) v


main :: IO ()
main = getLine >> getContents >>=
  mapM_ (putStrLn . show . length . filter isPalin .calc) . chunk . lines
  where chunk :: [String] -> [[String]]
        chunk [] = []
        chunk (x:xs) = let p = read x
                           (v,xs') = splitAt p xs
                       in v:chunk xs'
        isPalin x = x == reverse x
-}

-- Iterate it
{-
{-# OPTIONS_GHC -O2 #-}
import Data.List

nub' :: [Int] -> [Int]
nub' xs = go $ sort xs
  where go [] = []
        go [x] = [x]
        go (x:y:xs) | x == y = go (y:xs)
                    | otherwise = x:go(y:xs)
{-# INLINE nub' #-}
calc :: Int -> [Int] -> Int
calc n [] = n
calc n xs = calc (n+1) ys
  where ys = nub' [ abs (x-y) | xs' <- init (tails xs), y <- tail xs',
                           let x = head xs']

main = do
  a <- readLn :: IO Int
  l <- rl
  print $ calc 0 (nub' l)
  where rl = fmap (map read .words) getLine
-}
-- Surveillance
-- 0: 3, 1:
