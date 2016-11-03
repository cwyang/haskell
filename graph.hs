{-# Language BangPatterns #-}
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Control.Applicative
import Debug.Trace

type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]
type AdjMatrix a = Array (Vertex,Vertex) a

buildAdjMatrix :: (Int, Int) -> [Edge Int] -> AdjMatrix (Maybe Int)
buildAdjMatrix (a,b) edges = accumArray (flip const) Nothing ((a,a),(b,b)) edges'
  where edges' = map (\(a,b,w) -> ((a,b),Just w)) edges

floydWarshall :: AdjMatrix (Maybe Int) -> AdjMatrix (Maybe Int)
floydWarshall am = traceShow "doing" $ runST $ do
  arr <- thaw am :: ST s (STArray s (Vertex,Vertex) (Maybe Int))
  sequence_ [ go arr k i j | k <- r, i <- r, j <- r]
  freeze arr
  where ((minb,_), (maxb,_)) = bounds am
        r = [minb..maxb]
        go :: STArray s (Vertex,Vertex) (Maybe Int)
           -> Vertex -> Vertex -> Vertex -> ST s ()
        go arr k i j = do
          ij <- readArray arr (i,j)
          ik <- readArray arr (i,k)
          kj <- readArray arr (k,j)
          case (ik, kj) of
            (Nothing, _) -> return ()
            (_, Nothing) -> return ()
            (Just a, Just b) -> case ij of
              Nothing  -> do
                writeArray arr (i,j) $ Just (a+b)
              (Just c) -> when (c > a+b) $ do
                writeArray arr (i,j) $ Just (a+b)
        myMin Nothing x = x
        myMin x Nothing = x
        myMin x y = min x y

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

main :: IO ()
main = do
  [n,m] <- rl
  edges <- replicateM m $ do
    [from,to,weight] <- rl
    return (from,to,weight)
  [q] <- rl
  let am  = buildAdjMatrix (1,n) edges
      !res = floydWarshall am
--  forM_ [1..q] $ \_ -> do
  sequence_ $ replicate q $ do
    [start,end] <- rl
    putStrLn . show $ maybe (-1) id (res ! (start,end))
  where rl = map readInt . B.words <$> B.getLine
