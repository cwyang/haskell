{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE RankNTypes #-}
import Control.Monad.ST
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Array.ST
import Data.STRef

type Foo = MaybeT []

foo :: Foo Int
foo = return 1

type Bar = MaybeT (State String)

bar :: Bar Int
bar = return 1

type Baz1 = StateT String []

baz1 :: Baz1 Int
baz1 = return 1

--type Foo1 = STArray s Int Int -> MaybeT (ST s)

bar1 :: STArray s Int Int -> ST s Int
bar1 x = undefined

test1 = runST $ do
  x <- newArray (0,10) 0
  bar1 x

test2 = runST (newArray (0,10) 0 >>= bar1)

foo1 :: STArray s Int Int -> MaybeT (ST s) Int
foo1 x = undefined

test = runST (newArray (0,10) 0 >>= runMaybeT . foo1)


t :: Int
t = runST $ do
  ref <- newSTRef 10
  readSTRef ref

tt :: (forall s a. STRef s a -> ST s a) -> Int
tt f = runST $ do
  ref <- newSTRef 10
  f ref

ttTest = tt readSTRef
