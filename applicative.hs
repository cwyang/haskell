{-# LANGUAGE InstanceSigs #-}
import Data.Monoid
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import System.Random
--import Control.Monad.Trans.State
import Control.Monad.State
import Control.Monad (replicateM)
import qualified Data.DList as DL

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]
instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools
instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)

--foo = quickBatch $ applicative [("b", "w", 1)]
trigger = undefined :: [(String,String,Int)]
foo = quickBatch $ applicative [("b", "w", 1 ::Int)]
bar = quickBatch (applicative trigger)
baz = quickBatch (monad [("a","b","c")])

-- bad monad

data CountMe a = CountMe Integer a deriving (Eq, Show)
instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i+1) (f a)
instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n+n') (f a)
instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n+1) b
instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary
instance Eq a => EqProp (CountMe a) where (=-=) = eq

baar = let trigger = undefined :: CountMe (Int, String, Int) in
  quickBatch (functor trigger) >>
  quickBatch (applicative trigger) >>
  quickBatch (monad trigger)

type TI = []
baaz = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)

-- just excercises

hurr :: Num a => a->a
hurr = (*2)
durr :: Num a => a->a
durr = (+10)
m :: Num a => a->a
m = hurr . durr

myLiftA2 :: Applicative f =>
            (a->b->c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

newtype Reader' r a = Reader' { runReader :: r -> a }
instance Functor (Reader' r) where
  fmap f (Reader' ra) = Reader' $ (f . ra)
instance Applicative (Reader' r) where
  pure = Reader' . const
  (<*>) :: Reader' r (a->b) -> Reader' r a -> Reader' r b
  (Reader' rab) <*> (Reader' ra) = Reader' $ \r -> (rab r) (ra r)
instance Monad (Reader' r) where
  return = pure
  (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
  (Reader' ra) >>= arb = Reader' $ \r -> runReader (arb (ra r)) r
asks' :: (r -> a) -> Reader' r a
asks' f = Reader' f

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- uncurry :: (a->b->c) -> (a,b) -> c
-- fromMaybe :: a -> Maybe a -> a
xs, ys, zs :: Maybe Integer
xs = lookup 3 (zip x y)
ys = lookup 6 $ zip y z
zs = lookup 4 $ zip x y
z':: Integer -> Maybe Integer
z' n = lookup n $ zip x z
x1,x2 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys
x2 = liftA2 (,) ys zs
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'
summed::Num c => (c,c) -> c
summed = uncurry (+)
bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)
sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]
s' = summed <$>((,) <$> xs <*> ys)
testFoo :: IO ()
testFoo = do
  print $ sequence [Just 3, Just 2, Just 1]
  print $ sequence [x,y]
  print $ sequence [xs, ys]
  print $ summed <$> ((,)<$>xs<*>ys)
  print $ fmap summed ((,)<$>xs<*>zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ all id $ sequA 7
  print $ sequA <$> s'

rollDie :: State StdGen Int
rollDie = state $ do
  (n,s) <- randomR (1,6)
  return (n,s)
nDie :: Int -> State StdGen [Int]
nDie n = replicateM n rollDie
rollsToGetN :: Int -> StdGen -> (Int, [Int])
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> (Int, [Int])
        go sum count gen
          | sum >= n = (count, [])
          | otherwise = let (die, nextGen) = randomR (1,6) gen
                            (x,y) = go (sum + die) (succ count) nextGen
                        in (x, die:y)
testRoll = evalState (nDie 10) (mkStdGen 0)
testRoll2 = (rollsToGetN 20 . mkStdGen) <$> randomIO

newtype State' s a = State' { runState' :: s -> (a,s) }
instance Functor (State' s) where
  fmap f (State' s_as) = State' $ \s -> let (a,s') = s_as s
                                        in (f a, s')
instance Applicative (State' s) where
  pure x = State' $ \s -> (x,s)
  State' ab <*> State' a = State' $ \s -> let (ab', s') = ab s
                                              (a', s'') = a s'
                                          in (ab' a', s'')
instance Monad (State' s) where
  return = pure
  State' a >>= amb = State' $ \s -> let (a', s') = a s
                                    in runState' (amb a') s'
get' :: State' s s
get' = State' $ \s -> (s, s)
put' :: s -> State' s ()
put' sv = State' $ \s -> ((), sv)
exec' :: State' s a -> s -> s
exec' (State' sa) = snd . sa
eval' :: State' s a -> s -> a
eval' (State' sa) = fst . sa
modify'' :: (s -> s) -> State' s ()
modify'' f = State' $ \s -> ((), f s)
stfTest = runState' ((+1) <$> (State' $ \s -> (0,s))) 0
staTest = runState' ((State' $ \s -> (\x -> x + 1,s*10)) <*> (State' $ \s -> (0,s+3))) 1
stmTest = runState' ((State' $ \s -> (10, s+100)) >>= \a -> (State' $ \s -> (a*2, s*4))) 1
stgTest = runState' get' "Hello"
stpTest = runState' (put' "foo") "bar"
stxTest = exec' (put' "wilma") "daphne"
stzTest = runState' (modify'' (+1) >> modify'' (+1)) 0
-- How to generate Function randomly?

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15  == 0 = "FizzBuzz"
           | n `mod` 5   == 0 = "Fizz"
           | n `mod` 3   == 0 = "Buzz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result:xs)

fizz :: Integer -> Integer -> IO ()
fizz from to = mapM_ putStrLn $ fizzBuzzList [to,to-1..from]

             
