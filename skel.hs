module Skel where

-- $setup
-- >>> import Test.QuickCheck

-- | Returns the head of the list or the given default.
--
-- >>> myHead 0 [1,2,3]
-- 1
--
-- prop> myHead 0 [] == 0
-- prop> x `myHead` [] == x
myHead :: a -> [a] -> a
myHead x [] = x
myHead _ (y : _) = y

