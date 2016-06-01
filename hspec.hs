module Hspec where

import Test.Hspec
import Test.QuickCheck

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
test = do
  testFoo
  testBar
  testBar'
--  testSample
