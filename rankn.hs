{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Random
import Control.Monad.State

type GenAction m = forall a. (Random a) => m a

genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

randomInt :: (MonadIO m) => GenAction m -> m Int
randomInt gen = do
  liftIO $ putStrLn "Generating Int"
  a <- gen
  liftIO $ putStrLn "Done"
  return a

testRandom = randomInt randomIO >>= print
testRandom2 :: Int
testRandom2 = evalState (genRandom) (mkStdGen 0)

-- Scott Encoding

newtype ListS a =
  ListS {
  unconsS :: forall r. (a -> ListS a -> r) -> r -> r
  }

nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni l = (unconsS l) co ni

a = consS 99 $ consS 1 $ consS 2 nilS
runA = unconsS' unravel [] a
  where unravel x xs = x : unconsS' unravel [] xs
        
instance Functor ListS where
  fmap f = unconsS'(\x xs -> consS (f x) (fmap f xs)) nilS

-- Church Encoding
newtype ListC a =
  ListC {
  foldC :: forall r. (a -> r -> r) -> r -> r
  }

foldC' :: (a->r->r) -> r -> ListC a -> r
foldC' co ni (ListC f) = f co ni

nilC :: ListC a
nilC = ListC (\_ ni -> ni)

consC :: a -> ListC a -> ListC a
consC x xs = ListC  $ \co ni -> co x ((foldC xs) co ni) -- saw answer

instance Functor ListC where
  fmap f = foldC' (\x xs -> consC (f x) xs) nilC

unconsC :: (a -> ListC a -> r) -> r -> ListC a -> r
unconsC co ni (ListC f) = undefined

b = consC 99 $ consC 1 $ consC 2 nilC
runB = unconsC unravel [] b
  where unravel x xs = x : unconsC unravel [] xs

data ShowBox = forall s. Show s => SB {runShow::s}

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

instance Show ShowBox where
  show (SB s) = show s 

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

runHetero = f heteroList

data Box = forall a. Box a (a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show

apply :: Box -> String
apply (Box x f p) = p (f x)

-- 
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

runSBox = mapM_ (putStrLn . showBox) boxes

f1 x = show x
--f2 = show

plus = (+)
main = do
  print $ plus 1.2 3

ff x = const x gg
gg y = ff 'A'

data Tree a = Leaf | Bin a (Tree (a, a))

size Leaf = 0
size (Bin _ t) = 1 + 2 * size t
