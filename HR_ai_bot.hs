{-
-- BotClean.hs
module Main where
import Data.List
import Data.Function
getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

-- try 1 == try 3 with dumb findBoard
-- try 2
next_move :: String -> [String] -> String
next_move pos board'
  | dirty x y = "CLEAN"
  | y /= top && y /= bottom = decideY y top bottom
  | otherwise = decideX x left right
  where [y,x] = map (read :: String->Int) $ words pos
        board = map (map (\x -> if x == 'd' then True else False)) board'
        (top,bottom) = let l = findIndices id $ map (any id) board
                       in (head l, last l)
        (left,right) = let l = findIndices id $ head $ drop y board
                       in (head l, last l)
        d a b = abs (a - b)
        dirty x y = head $ drop x $ head $ drop y board
        decideX x left right
          | x < left = "RIGHT"
          | x > right = "LEFT"
          | d x left < d x right = "LEFT"
          | otherwise = "RIGHT"
        decideY y top bottom
          | y < top = "DOWN"
          | y > bottom = "UP"
          | d y top < d y bottom = "UP"
          | otherwise = "DOWN"
-- try 3
nextMove:: String -> [String] -> String
nextMove pos board = if (board !! y) !! x == 'd'
                     then "CLEAN"
                     else if abs dx > abs dy
                          then genMove dx "LEFT" "RIGHT"
                          else genMove dy "UP" "DOWN"
  where [y,x] = map read $ words pos
        (tx,ty) = head $ sortBy (compare `on` (dist (x,y))) $ findBoard 'd' 0 board
        (dx,dy) = (tx - x, ty - y)
        genMove distance dir1 dir2 = if distance > 0 then dir2
                                                     else dir1
        findBoard _ _ []         = []
        findBoard target y (h:t) = r ++ findBoard target (y+1) t
          where r = zip (elemIndices target h) (repeat y)
        dist (x,y) (a,b) = abs (a-x) + abs (b-y)
            
main :: IO()
main = do
    pos <- getLine
    board <- getList 5
    putStrLn $ next_move pos board

-- BotClean Large
module Main where
import Data.List (unfoldr, elemIndices, sortBy)
import Data.Function
import Debug.Trace
getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- splits a String by seperation character to list of Strings
split sep = takeWhile (not . null) . unfoldr (Just . span (/= sep) . dropWhile (== sep))

type Dim = (Int,Int)
type BotPos = (Int,Int)

nextMove :: BotPos -> Dim -> [String] -> String
nextMove bot@(y,x) dim board 
  | otherwise                = if (board !! y) !! x == 'd'
                               then "CLEAN"
                               else if abs dx > abs dy
                                    then genMove dx "LEFT" "RIGHT"
                                    else genMove dy "UP" "DOWN"
  where (tx,ty) = head $ sortBy (compare `on` (dist (x,y))) $ findBoard 'd' 0 board
        (dx,dy) = (tx - x, ty - y)
        genMove distance dir1 dir2 = if distance > 0 then dir2
                                                     else dir1
        findBoard _ _ []         = []
        findBoard target y (h:t) = r ++ findBoard target (y+1) t
          where r = zip (elemIndices target h) (repeat y)
        dist (x,y) (a,b) = abs (a-x) + abs (b-y)

main = do
  b <- getLine
  i <- getLine
  let bot = ((read (head s))::Int,(read (head(tail s))::Int)) where s = split (' ') b
  let dim = ((read (head s))::Int,(read (head(tail s))::Int)) where s = split (' ') i
  grid <- getList (fst dim)
  putStrLn $ nextMove bot dim grid
-}

-- BotClean Partially Observable
module Main where
import Data.List
import Data.Function
getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

next_move :: String -> [String] -> String
next_move pos board'
  | dirty x y = "CLEAN"
  | y /= top && y /= bottom = decideY y top bottom
  | otherwise = decideX x left right
  where [y,x] = map (read :: String->Int) $ words pos
        board = map (map (\x -> if x == 'd' then True else False)) board'
        (top,bottom) = let l = findIndices id $ map (any id) board
                       in (head l, last l)
        (left,right) = let l = findIndices id $ head $ drop y board
                       in (head l, last l)
        d a b = abs (a - b)
        dirty x y = head $ drop x $ head $ drop y board
        decideX x left right
          | x < left = "RIGHT"
          | x > right = "LEFT"
          | d x left < d x right = "LEFT"
          | otherwise = "RIGHT"
        decideY y top bottom
          | y < top = "DOWN"
          | y > bottom = "UP"
          | d y top < d y bottom = "UP"
          | otherwise = "DOWN"
main :: IO()
main = do
    pos <- getLine
    board <- getList 5
    putStrLn $ next_move pos board
