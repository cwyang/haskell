--https://gist.github.com/orclev/1929451
import qualified Data.Map.Strict as M
import Control.Monad (liftM)
import Data.Maybe
import qualified Data.Foldable as F

data Trie k v = Trie { value :: Maybe v
                     , children :: M.Map k (Trie k v) }
                deriving (Show)

type IntTrie = Trie Int ([Int], Bool)

instance F.Foldable (Trie a) where
  foldr f b t | isJust (value t)
                    = let thisNode = f (fromJust . value $ t) b
                          childNodes = ifold f
                      in F.foldr childNodes thisNode (children t)
              | otherwise
                    = F.foldr (ifold f) b (children t)
ifold :: F.Foldable t => (a->b->b) -> t a -> b -> b
ifold = flip. F.foldr

emptyTrie :: IntTrie
emptyTrie = Trie { value = Nothing
                 , children = M.empty}
--helper
trie :: [Int] -> IntTrie
trie k = emptyTrie { value = Just (k, False) }

setEnd :: Maybe ([Int], Bool) -> Maybe ([Int], Bool)
setEnd = liftM update
  where update (s,_) = (s, True)

insert :: [Int] -> IntTrie -> IntTrie
insert [] t     = t { value = setEnd $ value t }
insert (k:ks) t = let ts = children t
                      childNode = maybe (trie [k])
                        (trie . (++[k]) . fst)
                        (value t)
                      newChildren = M.insert k childNode ts
                  in case M.lookup k ts of
                         Nothing -> t { children = M.insert k (insert ks childNode) newChildren }
                         Just t' -> t { children = M.insert k (insert ks t') ts }

find :: [Int] -> IntTrie -> Maybe ([Int], Bool)
find s t = findPrefix s t >>= value

allPrefixes :: IntTrie -> [[Int]]
allPrefixes = map fst . filter snd . F.toList

findPrefix :: [Int] -> IntTrie -> Maybe IntTrie
findPrefix [] t     = Just t
findPrefix (k:ks) t = case M.lookup k (children t) of
                           Nothing -> Nothing
                           Just t' -> findPrefix ks t'

autoComplete :: [Int] -> IntTrie -> [[Int]]
autoComplete s t = maybe [] allPrefixes $ findPrefix s t
