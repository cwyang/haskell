{-# LANGUAGE BangPatterns #-}
{-
Preulde> :set -ddump-simpl
Prelude> :l Core.hs
Prelude> :set -dsuppress-all
Prelude> :r
Prelude> :sprint ..
-}

foo1, foo2, foo3, foo4, foo5 :: Int
foo1 = const 1 undefined
  -- = 1
foo2 = const undefined 1
  -- = undefined
foo3 = flip const undefined 1
  -- = 1
foo4 = flip const 1 undefined
  -- = undefined
foo5 = const undefined undefined
  -- = undefined
foo6, foo7 :: Char
foo6 = foldr const 'z' ['a'..'e']
  -- = 'a'
foo7 = foldr (flip const ) 'z' ['a'..'e']
  -- = 'z'

bar1 = Just ['a']
bar2 = Just "a"

lazyPattern :: (a,b)-> String
lazyPattern ~(a,b) = const "Counsin It" a

doesntEval :: Bool->Int
doesntEval b = 1
manualSeq :: Bool->Int
manualSeq b = b `seq` 1
banging :: Bool -> Int
banging !b = 1

data Foo = Foo Int !Int
first (Foo x _) = x
second (Foo _ y) = y

-- lazy in the spine, strict in the leaves

f :: Int->Int
f = \x -> x
x = f 1

x' = undefined
y' = "blah"
main' = do
  print (seq x' snd (x',y'))
