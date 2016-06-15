{-# LANGUAGE BangPatterns #-}
module StrictTest where

data List a = Nil
            | Cons !a !(List a) deriving Show

sTake :: Int -> List a -> List a
sTake n _
  | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = (Cons x (sTake (n-1) xs))

threeEls = Cons 2 twoEls
twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls
