{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PropertyBasedTest (
    propertyBasedTests,
) where

import Data.Hashable
import Lib
import Test.Invariant
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

newtype TestSepChainHashMap a b = TestSCHM {unTestSCHM :: SepChainHashMap a b} deriving (Semigroup, Monoid, Eq, Show)

instance (Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (TestSepChainHashMap a b) where
    arbitrary = TestSCHM <$> (createHashMap <$> filled <*> listOf arbitrary)
      where
        filled = choose (0.1, 0.9)

propertyBasedTests :: TestTree
propertyBasedTests =
    testGroup
        "Property-based tests"
        [ testProperty "binary operation is associative" (binOpAssociative :: TestSepChainHashMap Char Char -> TestSepChainHashMap Char Char -> TestSepChainHashMap Char Char -> Bool)
        , testProperty "binary operation with mempty not change elem" (binOpWithEmptyNotChange :: TestSepChainHashMap Char Char -> Bool)
        , testProperty "binary operation with mempty is commutative" (binWithEmptyCommutative :: TestSepChainHashMap Char Char -> Bool)
        , testProperty "currentFilled always less than inited" (currentFilledLessInited :: TestSepChainHashMap Char Char -> NonEmptyList (Char, Char) -> Bool)
        ]

binOpAssociative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> TestSepChainHashMap a b -> TestSepChainHashMap a b -> Bool
binOpAssociative = associative (<>)

binOpWithEmptyNotChange :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
binOpWithEmptyNotChange x = x == (x <> mempty)

binWithEmptyCommutative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
binWithEmptyCommutative = commutative (<>) mempty

currentFilledLessInited :: (Hashable a, Eq b) => TestSepChainHashMap a b -> NonEmptyList (a, b) -> Bool
currentFilledLessInited hM elems =
    snd $
        foldl
            ( \acc e ->
                let newMap = uncurry (addElem (fst acc)) e
                    checkFilled = getCurrentFilled newMap < filledHashMap newMap
                 in (newMap, snd acc && checkFilled)
            )
            (unTestSCHM hM, True)
            (getNonEmpty elems)
