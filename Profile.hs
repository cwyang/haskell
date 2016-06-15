-- time profile
-- stack ghc -- -prof -fprof-auto -rtsopts -O2 Profile.hs
-- ./Profile +RTS -P
-- cat Profile.prof

-- heap profile
-- ./Profile +RTS -hc -p
-- hp2ps Profile.hp

module Main where

import Control.Monad
{-
blah :: [Integer]
blah = [1..1000]

f,g :: IO ()
f = do
  print ([1..] !! 99999)
  putStrLn "f"

g = do
  print ([1..] !! 999999)
  putStrLn "g"

main2 :: IO ()
main2 = do
  f
  g
  replicateM_ 10000 (print blah)

-}
incIntOrg :: [Integer]
incInt = map (+1) [1..]

incInt :: a -> [Integer]  -- results IS shared. should it be?
incInt _ = map (+1) $ myEnum 0 1
{-# NOINLINE incInt #-}
myEnum :: a -> Integer -> [Integer]
myEnum _ n =  enumFrom n
{-# NOINLINE myEnum #-}
main :: IO ()
main = do
  print (incInt 0 !!  9999999)
  print (incInt 0 !!  9999999)
  print (incInt 0 !!  9999999)
  
