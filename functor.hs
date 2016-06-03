import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Maybe
import Data.Monoid
import Control.Applicative hiding (ZipList)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) =>
                  f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IdT1 = Identity Int -> Bool
type IdT2 = Identity Int -> IntToInt -> IntToInt -> Bool
idTest' = do
  quickCheck (functorIdentity :: IdT1)
  quickCheck (functorCompose :: IdT2)

-- Laws
{-
 Functor Identity:     fmap id == id
 Functor Composition:  fmap (f . g) == fmap f . fmap g
 Applica Identity:     pure id <*> v = v
 Applica Composition:  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
 Applica Homomorphism: pure f <*> pure x = pure (f x)
 Applica Interchange:  u <*> pure y = pure ($ y) <*> u
 Monad R.Identity:     m >>= return   == m
 Monad L.Identity:     return x >>= f == f x
 Monad Associativity:  (m >>= f) >>= g == m >>= (\x -> f x >>= g)
-}

-- Nope
data Nope a = Nope deriving (Show, Eq)
instance Functor Nope where
  fmap _ Nope = Nope
instance Applicative Nope where
  pure _ = Nope
  _ <*> _ = Nope
instance Monad Nope where
  return = pure
  _ >>= _ = Nope
instance EqProp (Nope a) where (=-=) = eq
instance Arbitrary (Nope a) where arbitrary = elements [Nope]
nopeTest = do
  let trigger = undefined :: Nope (Int,String,Int)
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (monad trigger)

-- Identity
newtype Identity a = Identity a deriving (Show, Eq)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x
instance Monad Identity where
  return = pure
  Identity x >>= f = f x
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = Identity <$> arbitrary
idTest = do
  let trigger = undefined :: Identity (Int,String,Int)
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (monad trigger)

-- Constant
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant f) where
  fmap _ (Constant v) = Constant v
instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant $ x <> y
instance Eq a => EqProp (Constant a b) where (=-=) = eq
instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = constantGen
constantGen :: Arbitrary a => Gen (Constant a b)
constantGen = Constant <$> arbitrary
constTest = do
  let trigger = undefined :: Constant String (Int,String,Int)
  quickBatch (functor trigger)
  quickBatch (applicative trigger)

-- ZipList
newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Show)
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ map f xs
instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList $ repeat mempty
  -- mappend = liftA2 mappend  -- if Applicative is declared first
  (ZipList a) `mappend` (ZipList b) =
    ZipList $ zipWith mappend a b
instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList f <*> ZipList x = ZipList $ zipWith id f x
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
instance Eq a => EqProp (ZipList a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList l) = xs in take 100 l
          ys' = let (ZipList l) = ys in take 100 l
zipListTest = do
  let trigger = undefined :: ZipList (Int,String,Int)
  quickBatch (functor trigger)
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch (applicative trigger)

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)
toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs
append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)  -- foldr (not foldl)
 -- foldl --> fold f (f b h) t
concat' :: List (List a) -> List a
concat' = fold append Nil
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f
instance Monoid a => Monoid (List a) where
  mempty = Nil
  mappend = append
instance Functor List where
  _ `fmap` Nil = Nil
  f `fmap` Cons x xs = Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)
instance Monad List where
  return = pure
  Nil >>= f = Nil
  x >>= f = flatMap f x

instance Eq a => EqProp (List a) where (=-=) = eq
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toList <$> (arbitrary :: Arbitrary a => Gen [a])
listTest = do
  let trigger = undefined :: List (Int,String,Int)
  quickBatch (functor trigger)
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch (monad trigger)  -- SLOW!!!!
  quickBatch (applicative trigger) -- SLOW!!!

-- Either variant
data Sum' a b = First' a | Second' b deriving (Eq, Show)
data Validation e a = Error' e | Success' a deriving (Eq, Show)
instance Functor (Sum' a) where
  fmap _ (First' x) = First' x
  fmap f (Second' x) = Second' $ f x
instance Applicative (Sum' a) where
  pure = Second'
  First' x <*> _ = First' x
  _ <*> First' x = First' x
  Second' f <*> Second' x = Second' $ f x
instance Monad (Sum' a) where
  return = pure
  First' x >>= _ = First' x
  Second' x >>= f = f x
instance Functor (Validation e) where
  fmap _ (Error' x) = Error' x
  fmap f (Success' x) = Success' $ f x
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Error' x <*> Error' y = Error' $ x <> y
  Error' x <*> _ = Error' x
  _ <*> Error' x = Error' x
  Success' f <*> Success' x = Success' $ f x
instance (Eq a, Eq b) => EqProp (Sum' a b) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = do
    (a,b) <- arbitrary
    oneof [return $ First' a, return $ Second' b]
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    (a,b) <- arbitrary
    oneof [return $ Error' a, return $ Success' b]
eitherTest = do
  let t1 = undefined :: Sum' Int (Int,String,Int)
      t2 = undefined :: Validation (Sum Int) (Int,String,Int)
  quickBatch (functor t1)
  quickBatch (applicative t1)
  quickBatch (monad t1)
  quickBatch (functor t2)
  quickBatch (applicative t2)
--  quickBatch $ monoid (ZipList [1 :: Sum Int])
  
-- Misc

data Pair a = Pair a a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary; b <- arbitrary
  return $ Two a b
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen
type TwoT1 = Two Int Int -> Bool
type TwoT2 = Two Int Int -> IntToInt -> IntToInt -> Bool
twoTest = do
  quickCheck (functorIdentity :: TwoT1)
  quickCheck (functorCompose :: TwoT2)

maybeToList Nothing = []
maybeToList (Just a) = [a+1]
