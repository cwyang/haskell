module Main where
import Data.List
getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess n board = intercalate "\n" $ genMove (tx - x) "LEFT" "RIGHT" ++ genMove (ty - y) "UP" "DOWN"
  where (x,y)   = findBoard 'm' 0 board
        (tx,ty) = findBoard 'p' 0 board
        genMove distance dir1 dir2 = if distance > 0 then replicate distance dir2
                                                     else replicate (-distance) dir1
        findBoard _ _ []         = error "no target in board"
        findBoard target y (h:t) = case elemIndex target h of
                                        Just x -> (x,y)
                                        Nothing -> findBoard target (y+1) t

testData = ["---","---","d-m"]
foo = displayPathtoPrincess 3 testData
bar = nextMove "2 0" testData

nextMove:: String -> [String] -> String
nextMove pos board = if (board !! y) !! x == 'd'
                     then "CLEAN"
                     else if abs dx > abs dy
                          then genMove dx "LEFT" "RIGHT"
                          else genMove dy "UP" "DOWN"
  where [y,x] = map read $ words pos
        (tx,ty) = findBoard 'd' 0 board
        (dx,dy) = (tx - x, ty - y)
        genMove distance dir1 dir2 = if distance > 0 then dir2
                                                     else dir1
        findBoard _ _ []         = error "no target in board"
        findBoard target y (h:t) = case elemIndex target h of
                                        Just x -> (x,y)
                                        Nothing -> findBoard target (y+1) t

main = do
    pos <- getLine
--    xy <- getLine
--    let i = read n
    grid <- getList 5
    putStrLn $ nextMove pos grid
