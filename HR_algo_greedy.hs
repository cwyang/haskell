{-
-- Max Min
import Data.List

data Queue a = Queue [a] [a]
empty :: Queue a
empty = Queue [] []
push :: a -> Queue a -> Queue a
push a (Queue x y) = Queue (a:x) y
pop :: Queue a -> (a, Queue a)
pop (Queue x (y:ys)) = (y, Queue x ys)
pop (Queue [] []) = error "cannot pop from empty queue"
pop (Queue x []) = pop (Queue [] (reverse x))
lastQ :: Queue a -> a
lastQ (Queue (a:x) _) = a
lastQ (Queue [] []) = error "empty queue"
lastQ (Queue [] x) = lastQ (Queue (reverse x) [])
qToList :: Queue a -> [a]
qToList (Queue x y) = y ++ reverse x

qVal :: Queue Int -> Int
qVal q = lastQ q - fst (pop q)

scanList :: Int -> [Int] -> Int
scanList k l = let (ini, rst) = splitAt k l
                   initQ = foldl' (\acc x -> push x acc) empty ini
                   go (minQ,curQ) x = let (_,nextQ) = pop $ push x curQ
                                      in if (qVal minQ > qVal nextQ)
                                         then (nextQ, nextQ)
                                         else (minQ, nextQ)
               in qVal $ fst $ foldl' go (initQ, initQ) rst
-}      
{-
-- Largest Permutation
go :: Int -> [Int] -> [Int]
go 0 x = x
go _ [] = []
go k l@(x:xs) = let m = maximum l
                    (a,b) = break (== m) xs in
                if m == x then x : go k xs
                else m : go (k-1) (a++(x:(tail b)))
main = do
    l1 <- getLine
    l2 <- getLine
    let [_,k] = map read . words $ l1
        l = map read . words $ l2
    putStrLn . unwords . map show . go k $ l
-}

-- Cutting Boards

 go :: ((Int,Int),([Int],[Int])) -> Int
 go _ = 1
 readArgs :: [String] -> [((Int,Int),([Int],[Int]))]
 readArgs [] = []
 readArgs (x:y:z:xs) =
   let mr = map read . words
       [m,n] = mr x
       y' = mr y
       z' = mr z
   in ((m,n),(y',z')):readArgs xs
 main = getLine >> getContents >>=
   mapM_ (putStrLn . show . go) . readArgs . lines 
