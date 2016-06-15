module Main where 
import Criterion.Main
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Sequence ((<|), (|>))
newtype DList' a = DL' { unDL' :: [a] -> [a] }

empty :: DList' a
empty = DL' $ id
{-# INLINE empty #-}

singleton :: a -> DList' a
singleton x = DL' $ (x:)
{-# INLINE singleton #-}

toList' :: DList' a -> [a]
toList' dl = unDL' dl $ []
{-# INLINE toList' #-}

infixr `cons`
cons :: a -> DList' a -> DList' a
cons x xs = DL' ((x:) . unDL' xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList' a -> a -> DList' a
snoc xs x = DL' (unDL' xs . (x:))
{-# INLINE snoc #-}

append :: DList' a -> DList' a -> DList' a
append x y = DL' (x' . y')
  where x' = unDL' x
        y' = unDL' y
{-# INLINE append #-}

foo = cons 1 (cons 2 ( cons 3 empty))
foo' = cons 4 (cons 3 ( cons 2 empty))
bar = (snoc (snoc (snoc (snoc empty 5) 4) 3) 2)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n]++xs)

constructDlist :: Int -> [Int]
constructDlist i = toList' $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main1 :: IO ()
main1 = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]


class Queue q where
  nil :: q a
  push :: a -> q a -> q a
  pop :: q a -> Maybe (a, q a)
  toList :: q a -> [a]
  last :: q a -> Maybe a

data TwoQueue a = TwoQueue { enqueue :: [a]
                           , dequeue :: [a]
                           } deriving (Eq, Show)

instance Queue TwoQueue where
  nil = TwoQueue [] []
  {-# INLINE nil #-}
  push a (TwoQueue x y) = TwoQueue (a:x) y
  {-# INLINE push #-}
  pop (TwoQueue x (y:ys)) = Just (y, TwoQueue x ys)
  pop (TwoQueue [] []) = Nothing
  pop (TwoQueue x []) = pop (TwoQueue [] (reverse x))
  {-# INLINE pop #-}
  toList (TwoQueue x y) = y ++ reverse x
  {-# INLINE toList #-}
  last (TwoQueue (a:x) _) = Just a
  last (TwoQueue [] []) = Nothing 
  last (TwoQueue [] x) = Main.last (TwoQueue (reverse x) [])

data OneQueue a = OneQueue { runQueue :: [a]
                           } deriving (Eq, Show)

instance Queue OneQueue where
  nil = OneQueue []
  {-# INLINE nil #-}
  push a (OneQueue x) = OneQueue (a:x)
  {-# INLINE push #-}
  pop (OneQueue []) = Nothing
  pop (OneQueue ys) = Just (Prelude.last ys, OneQueue $ init ys)
  {-# INLINE pop #-}
  toList (OneQueue x) = x
  {-# INLINE toList #-}
  last (OneQueue []) = Nothing 
  last (OneQueue x) = Just (Prelude.last x)

data SeqQueue a = SeqQueue { runSeq :: S.Seq a
                           } deriving (Eq, Show)
instance Queue SeqQueue where
  nil = SeqQueue S.empty
  {-# INLINE nil #-}
  push a (SeqQueue x) = SeqQueue (x |> a)
  {-# INLINE push #-}
  pop (SeqQueue ys) | S.null ys = Nothing
  pop (SeqQueue ys) = Just (S.index ys 0, SeqQueue $ S.drop 1 ys)
  {-# INLINE pop #-}
  toList (SeqQueue x) = F.toList x
  {-# INLINE toList #-}
  last (SeqQueue ys) | S.null ys = Nothing 
  last (SeqQueue ys) = Just (S.index ys 0)

d :: [Int]
d = [1..1000]
twoqueue :: TwoQueue Int
twoqueue = foldl' (\acc x -> push x acc) nil d
onequeue :: OneQueue Int
onequeue = foldl' (\acc x -> push x acc) nil d
seqqueue :: SeqQueue Int
seqqueue = foldl' (\acc x -> push x acc) nil d

test:: Queue q => q Int -> [Int]
test q = toList $ foldr (\x acc -> let (_,q') = fromJust (pop acc) in push x q') q d
main :: IO ()
main = defaultMain
  [ bench "pushpop twoqueue" $ nf test twoqueue
  , bench "pushpop onequeue" $ nf test onequeue
  , bench "pushpop seqqueue" $ nf test seqqueue
  ]
