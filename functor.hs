{-# LANGUAGE FlexibleContexts #-}
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Control.Applicative hiding (ZipList)
import Test.Hspec
import Data.Char

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
 Traverse Naturality:  t . traverse f = traverse (t . f)
 Traverse Identity:    traverse Identity = Identity
 Traverse Composition: traverse (Compose . fmap g . f) =
                         Compose . fmap (traverse g) . traverse f
 This is easier: The law of composition
   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
 sequenceA Naturality: t . sequenceA = sequenceA . fmap t
 sequenceA Identity:   sequenceA . fmap Identity = Identity
 sequenceA Composition:sequenceA . fmap Compose =
                         Compose . fmap sequenceA . sequenceA
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
newtype Identity a = Identity a deriving (Show, Eq, Ord)
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x
instance Monad Identity where
  return = pure
  Identity x >>= f = f x
instance Foldable Identity where
  foldMap f (Identity x) = f x 
  foldr f e (Identity x) = f x e
instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = Identity <$> f x
  sequence (Identity x) = Identity <$> x
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = Identity <$> arbitrary
idTest = do
  let trigger = undefined :: Identity (Int,String,[Int])
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (monad trigger)
  quickBatch (traversable trigger)

-- Constant
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant f) where
  fmap _ (Constant v) = Constant v
instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant $ x <> y
instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty
instance Traversable (Constant a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Constant x) = pure $ Constant x
  sequence (Constant x) = pure $ Constant x
instance Eq a => EqProp (Constant a b) where (=-=) = eq
instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = constantGen
constantGen :: Arbitrary a => Gen (Constant a b)
constantGen = Constant <$> arbitrary
constTest = do
  let trigger = undefined :: Constant String ([Int],String,[Int])
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
  
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
toList' :: [a] -> List a
toList' [] = Nil
toList' (x:xs) = Cons x $ toList' xs
append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys
fold' :: (a -> b -> b) -> b -> List a -> b
fold' _ b Nil = b
fold' f b (Cons h t) = f h (fold' f b t)  -- foldr (not foldl)
 -- foldl --> fold f (f b h) t
concat' :: List (List a) -> List a
concat' = fold' append Nil
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
instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs 
  foldr f e Nil = e
  foldr f e (Cons x xs) = f x (foldr f e xs)
instance Traversable List where
  -- traverse :: Applicative f & traversable t => (a -> f b) -> t a -> f (t b)
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)
  sequence Nil = pure Nil
  sequence (Cons x xs) = Cons <$> x <*> (sequence xs)
instance Eq a => EqProp (List a) where (=-=) = eq
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toList' <$> (arbitrary :: Arbitrary a => Gen [a])
listTest = do
  let trigger = undefined :: List (Int,String,[Int])
  quickBatch (functor trigger)
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch (traversable trigger)
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
instance Foldable (Sum' a) where
  -- foldMap' :: (Foldable t, Monoid m) => (a->m) -> t a -> m
  foldMap fam ta@(First' x) = mempty
  foldMap fam ta@(Second' a) = fam a
  foldr fab e ta@(First' x) = e
  foldr fab e ta@(Second' b) = fab b e
instance Traversable (Sum' a) where
  -- traverse :: Applicative f & traversable t => (a -> f b) -> t a -> f (t b)
  traverse afb (First' x) = pure (First' x)
  traverse afb ta@(Second' a) = Second' <$> afb a
  sequence tma@(First' x) = pure $ First' x
  sequence tma@(Second' a) = Second' <$> a
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
  let t1 = undefined :: Sum' Int (Int,String,[Int])
      t2 = undefined :: Validation (Sum Int) (Int,String,Int)
  quickBatch (functor t1)
  quickBatch (applicative t1)
  quickBatch (monad t1)
  quickBatch (traversable t1)
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
instance Foldable (Two a) where
  foldMap f (Two a b) = f b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen
type TwoT1 = Two Int Int -> Bool
type TwoT2 = Two Int Int -> IntToInt -> IntToInt -> Bool
twoTest = do
  quickCheck (functorIdentity :: TwoT1)
  quickCheck (functorCompose :: TwoT2)

data Three' a b = Three' a b b deriving (Show, Eq)
threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  a <- arbitrary; b <- arbitrary; c <- arbitrary
  return $ Three' a b c
instance Functor (Three' a) where
  fmap f (Three' x a1 a2) = Three' x (f a1) (f a2)
instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = (f b1) <> (f b2)
instance Traversable (Three' a) where
  -- traverse :: Applicative f & traversable t => (a -> f b) -> t a -> f (t b)
  traverse afb ta@(Three' x a1 a2) = Three' x <$> afb a1 <*> afb a2
  sequence tfa@(Three' x a1 a2) = Three' x <$> a1 <*> a2
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

type ThreeT1' = Three' Int Int -> Bool
type ThreeT2' = Three' Int Int -> IntToInt -> IntToInt -> Bool
threeTest = do
  let trigger = undefined :: Three' Int (Int, Int, [Int])
  quickCheck (functorIdentity :: ThreeT1')
  quickCheck (functorCompose :: ThreeT2')
  quickBatch $ traversable trigger
  
type TI = [] -- traversable List instance
travTest = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch $ traversable trigger

data S n a = S (n a) a deriving (Eq, Show)  -- n has kind *->* for (n a) 
instance Functor n => Functor (S n) where
  fmap ab (S ma a) = S (ab <$> ma) (ab a)
instance Foldable n => Foldable (S n) where
 -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap am ta@(S na a) = foldMap am na <> am a
  foldr f e (S na a) = foldr f (f a e) na
instance Traversable n => Traversable (S n) where
  -- (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse afb ta@(S na a) = S <$> traverse afb na <*> afb a 
  sequence tma@(S na a) = S <$> sequence na <*> a
sGen' :: (Arbitrary a, Arbitrary (n a)) => Gen (S n a)
sGen' = do
  a <- arbitrary; b <- arbitrary; 
  return $ S a b
instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = sGen'
instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq
sTest = do
  let trigger = undefined :: S [] (Int, Int, [Int])
  quickBatch $ traversable trigger
  
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
instance Foldable Tree where
  foldMap am Empty = mempty
  foldMap am (Leaf a) = am a
  foldMap am (Node l a r) = foldMap am l <> am a <> foldMap am r
  foldr ab e Empty = e
  foldr ab e (Leaf a) = ab a e
  foldr ab e (Node l a r) = foldr ab (ab a fr) l
    where fr = foldr ab e r
instance Traversable Tree where
  -- (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse afb Empty = pure Empty
  traverse afb (Leaf a) = Leaf <$> afb a
  traverse afb (Node l a r) = Node <$> traverse afb l <*> afb a <*> traverse afb r

t1,t2,t3,t4 :: Tree (Int)
t1 = Empty
t2 = Leaf 2
t3 = Node t1 3 t2
t4 = Node t2 4 t3
-- misc funcs

join' :: Monad m => m (m a) -> m a
join' x = x >>= id
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = return . f =<< x
l1' f x = do
  x1 <- x
  return $ f x1
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y= let mf = l1 f x
          in mf >>= \f' -> return . f' =<< y
l2' f x y = do
  x1 <- x
  y1 <- y
  return $ f x1 y1
ap' :: Monad m => m a -> m (a -> b) -> m b
ap' ma mf = do
  f <- mf
  a <- ma
  return $ f a
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f)
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
  
funTest = hspec $ do
  describe "monad funcs" $ do
    it "join [[Int]]" $ do
      property $ \x -> join' (x :: [[Int]]) == concat x
    it "join Maybe(Maybe Int)" $ do
      property $ \x -> x == Nothing ||
                       (return . join') (x :: Maybe(Maybe Int)) == x
    it "liftM [Int]" $ property (prop_liftM :: [Int] -> (Fun Int Int) -> Bool)
    it "liftM Maybe Int" $ property (prop_liftM :: Maybe Int -> (Fun Int Int) -> Bool)
    it "ap [Int]" $ property (prop_ap :: [Int] -> (Fun Int Int) -> Bool)
    it "ap Maybe Int" $ property (prop_ap :: Maybe Int -> (Fun Int Int) -> Bool)
    it "meh Maybe" $ property (prop_mehMaybe :: [Int] -> (Fun Int Int) -> Bool)
    it "meh []" $ property (prop_mehList :: [Int] -> (Fun Int Int) -> Bool)
    it "flipType Maybe" $ do
      property $ \x -> let x' = filter (/= Nothing) x
                       in flipType x == sequence (x :: [Maybe Int])
--    it "flipType []" $ property $ \x -> flipType x == sequence (x :: [[Int]])    
    
--    it "liftM Maybe" $ property prop_liftM
prop_liftM :: (Monad m, Eq (m a)) => m a -> Fun a a -> Bool
prop_liftM = \x (Fun _ f) -> l1 f x == liftA f x
prop_ap :: (Monad m, Eq (m a)) => m a -> Fun a a -> Bool
prop_ap = \x (Fun _ f) -> ap' x (return f) == (return f <*> x)
prop_mehMaybe :: [Int] -> Fun Int Int -> Bool
prop_mehMaybe = \x (Fun _ f) -> let g = return . f :: Int -> Maybe Int
                           in meh x g == mapM g x
prop_mehList :: [Int] -> Fun Int Int -> Bool
prop_mehList= \x (Fun _ f) -> let g = return . f :: Int -> [Int]
                           in meh x g == mapM g x

--prop_liftM = \x (Fun _ f) -> l1 f x == liftA (f :: Int->Int) (x :: [Int])
-- quickCheck( prop_liftM :: [Int] -> (Fun Int Int) -> Bool)

sum' :: (Foldable t, Num a) => t a -> a
sum' ta = getSum $ foldMap (\x -> Sum x) ta
product' :: (Foldable t, Num a) => t a -> a
product' ta = getProduct $ foldMap (\x -> Product x) ta
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e xs = getAny $ foldMap (\x -> Any (x==e)) xs
maximum', minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = _subMinMax' min
maximum' = _subMinMax' max
_subMinMax' :: (Foldable t, Ord a) => (a->a->a) -> t a -> Maybe a
_subMinMax' f = foldr go Nothing
  where go x Nothing = Just x
        go x (Just y) = Just (f x y)
null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (\x -> All False)
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const $ Sum 1)
toList'' :: (Foldable t) => t a -> [a]
toList'' = foldr (\x acc -> x:acc) []
fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldMap id
foldMap' :: (Foldable t, Monoid m) => (a->m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty 
filterF :: (Applicative f, Foldable t, Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

