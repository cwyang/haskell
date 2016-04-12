import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

newtype MultiSet = MultiSet {run::(M.Map Int Int)}
  deriving (Show)

emptyMS :: MultiSet
emptyMS = MultiSet (M.empty)

insertMS :: MultiSet -> Int -> MultiSet
insertMS (MultiSet m) n =
  MultiSet $ M.insertWith (+) n 1 m
{-# INLINE insertMS #-}

findMaxMS :: MultiSet -> (Int, Int)
findMaxMS (MultiSet m) =  M.findMax m

deleteMaxMS :: MultiSet -> MultiSet
deleteMaxMS ms@(MultiSet m) =
  let (n,v) = findMaxMS ms
  in case v of
    1 -> MultiSet $ M.deleteMax m
    otherwise -> MultiSet $ M.insert n (v-1) m
{-# INLINE deleteMaxMS #-}

combineMS :: MultiSet -> MultiSet -> MultiSet
combineMS (MultiSet a) (MultiSet b) =
  MultiSet $ M.union a b

process :: [B.ByteString] -> [B.ByteString]
process = map (B.pack . show) . reverse . snd . foldl' go (M.empty,[])

go :: (M.Map Int (MultiSet,[Int]), [Int]) -> B.ByteString -> (M.Map Int (MultiSet,[Int]), [Int])
go (m, res) str =
  m `seq`
  case cmd of
    1 -> (M.insert armyNo (nor_h `seq` nor_h,[]) m,  fval `seq` fval : res)       -- find strongest
    2 -> (M.insert armyNo (dval `seq` dval, []) m, res) -- strongest died
    3 -> (M.insert armyNo (h `seq` h, auxArg:l) m, res) -- recruit
    4 -> (M.insert armyNo (h `seq` h' `seq` combineMS h h', l++l') (M.delete auxArg m), res) -- merge
  where (cmd:args) = map (fst . fromJust . B.readInt) . B.words $ str
        armyNo = head args
        auxArg = head . tail $ args
        (h,l) = case M.lookup armyNo m of
                Nothing -> (emptyMS, [])
                Just x  -> x
        (h',l') = case M.lookup auxArg m of
                Nothing -> (emptyMS, [])
                Just x  -> x
        nor_h = h `seq` MultiSet (foldl' go (run h) l)
        nor_h' = h' `seq` MultiSet (foldl' go (run h') l')
        go acc x = M.insertWith (+) x 1 acc
        fval = fst (findMaxMS nor_h)
        dval = deleteMaxMS nor_h
main = do
  l1 <- B.getLine
  let [n,q] = map (fromJust . B.readInt) . B.words $ l1
  input <- B.getContents
  B.putStrLn . B.unlines . process . B.lines $ input
