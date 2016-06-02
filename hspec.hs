{-# LANGUAGE DeriveGeneric #-} -- for coarbitrary
module Hspec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Function
import GHC.Generics -- for coarbitrary

import Data.List(sort)

data Sum a b = First a | Second b deriving (Eq, Show)

data Bool' = True' | False' deriving (Generic)
instance CoArbitrary Bool'

trueGen, falseGen :: Gen Int -- 이해못함.
trueGen = coarbitrary True' arbitrary
falseGen = coarbitrary False' arbitrary

testFoo = hspec $ do
  describe "Addition" $ do
    it "1 + 1 should be greater than 1" $ do
      (1+1) > 1 `shouldBe` True
    it "2 + 2 should be equal to 4" $ do
      (2+2)  `shouldBe` 4

testBar = hspec $ do
  describe "Sample QuickCheck" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
testBar' = quickCheck prop_additionGreater -- QuickCheck without hspec

testSample = do
  print =<< sample' (arbitrary :: Gen Int)
  sample (arbitrary :: Gen Int)

genBool, genBool' :: Gen Bool
genBool = choose (False, True)
genBool' = elements [False, True]
genChar :: Gen Char
genChar = elements ['a'..'z']
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)
genString :: Gen String
genString = do
  l <- fmap abs (arbitrary :: Gen Int)
  sequence $ map (\_ -> genChar) [1..l]
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary; b <- arbitrary
  elements [Left a, Right b]
genMaybe, genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe = do -- equal prob
  a <- arbitrary
  elements [Nothing, Just a]
genMaybe' = do -- more Just
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
genSum :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSum = do
  a <- arbitrary; b <- arbitrary
  oneof [return $ First a,  -- frequency may be OK
         return $ Second b]

genPosInt :: Gen Int
genPosInt = arbitrary >>= (return . succ . abs)

test = do
  testFoo
  testBar
  testBar'
--  testSample

--------
half x = x / 2
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == ((*2) . half) x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered x = listOrdered $ sort x

plusAssociative x y z = x+(y+z) == (x+y)+z
prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z = plusAssociative x y z

prop_quotRem :: NonZero Int -> NonZero Int -> Bool
prop_quotRem (NonZero x) (NonZero y) = (quot x y)*y + (rem x y) == x
prop_divMod :: NonZero Int -> NonZero Int -> Bool
prop_divMod (NonZero x) (NonZero y) = (div x y)*y + (mod x y) == x
prop_revrev :: (Eq a) => [a] -> Bool
prop_revrev x = x == (reverse . reverse $ x)
prop_fun :: (Eq b, Num a, Num b) => Fun a b -> a -> Bool
prop_fun (Fun _ f) x = f x /= (f $ x)
prop_concat :: Property
prop_concat =
  forAll (arbitrary :: Gen [[Int]])
  (\x -> foldr (++) [] x == concat x)
fs :: (Read a, Show a) => a -> a
fs = read . show 
prop_show :: (Read a, Show a, Eq a) => a -> Bool
prop_show x = fs x == x
