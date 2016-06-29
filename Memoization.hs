-- http://stackoverflow.com/questions/3208258/memoization-in-haskell#_=_
{-# LANGUAGE BangPatterns #-}
import Data.Function (fix)

data Tree a = Tree (Tree a) a (Tree a)
instance Functor Tree where
  fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n-1) `divMod` 2 of
  (q,0) -> index l q
  (q,1) -> index r q

nats :: Tree Int
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
      where
        l = n + s
        r = l + s
        s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0..]

f :: (Int -> Integer) -> Int -> Integer
f fib 1 = 1
f fib 2 = 1
f fib n = fib (n-1) + fib (n-2)

slow_fib = fix f

fib_list :: [Integer]
fib_list = map (f faster_fib) [0..]

faster_fib :: Int -> Integer
faster_fib n
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = (fib_list !! (n-1)) + (fib_list !! (n-2))

fib_tree :: Tree Integer
fib_tree = fmap (f fastest_fib) nats

fastest_fib :: Int -> Integer
fastest_fib n
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = (index fib_tree (n-1)) + (index fib_tree (n-2))

