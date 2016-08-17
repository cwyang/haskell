-- Coupling Passions
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Data.Monoid
constructMap :: IO (M.Map String Int)
constructMap = do
  n <- readLn :: IO Int
  l <- flip mapM [1..n] $ \_ -> do
    x <- getLine
    return $ zip (tail $ words x) [1,1..]
  return $ M.fromList (concat l)
type Entry = (String, (Double,Double), [String])
type Score = (Int, Double)
choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:xs) = map (\y -> (x,y)) xs ++ choose2 xs
calc :: M.Map String Int -> [(Entry,Entry)] -> [(Score, String,String)]
calc m = map go
  where go ((n1,(lat1,long1),pas1),(n2,(lat2,long2),pas2))
          = let p = sum $ map (\x -> maybe 0 id $ M.lookup x m) (nub (pas1 ++ pas2))
                d = distanceBetween (lat1,long1) (lat2,long2)
            in ((p,d), n1, n2)

distanceBetween :: (Double, Double) -> (Double, Double) -> Double
distanceBetween (lat1,long1) (lat2, long2)
  = acos( sin rlat1 * sin rlat2 + cos rlat1 * cos rlat2 * cos (rlong2 - rlong1))
  where (rlat1,rlat2,rlong1,rlong2) = (d2r lat1, d2r lat2, d2r long1, d2r long2)
        d2r :: Double -> Double
        d2r d = d / 180 * pi
main = do
  m <- constructMap
  n <- readLn :: IO Int
  l <- flip mapM [1..n] $ \_ -> do
    (name:lat:long:_:passions) <- fmap words getLine
    return (name, (read lat,read long), passions)
  let l' = choose2 l
      (_,n1,n2) = head $ sortBy compScore $ calc m l'
      compScore ((a,b),_,_) ((c,d),_,_) = flip compare a c <> compare b d
  putStrLn . unwords . sort $ [n1,n2]



  
