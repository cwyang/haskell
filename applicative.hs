{-# LANGUAGE InstanceSigs #-}
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

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
asks' :: (r -> a) -> Reader' r a
asks' f = Reader' f
