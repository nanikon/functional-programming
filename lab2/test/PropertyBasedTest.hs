{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PropertyBasedTest (
    runPropertyBasedTest,
) where

import Data.Hashable
import Lib
import Test.Invariant
import Test.QuickCheck

newtype TestSepChainHashMap a b = TestSCHM {unTestSCHM :: SepChainHashMap a b} deriving (Semigroup, Monoid, Eq, Show)

instance (Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (TestSepChainHashMap a b) where
    arbitrary = TestSCHM <$> (createHashMap <$> filled <*> listOf arbitrary)
      where
        filled = choose (0.1, 0.9)

prop_binaryOperationAssociative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> TestSepChainHashMap a b -> TestSepChainHashMap a b -> Bool
prop_binaryOperationAssociative = associative (<>)

prop_binaryOperationWithEmptyNotChange :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
prop_binaryOperationWithEmptyNotChange x = x == (x <> mempty)

prop_binaryOperationWithEmptyCommutative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
prop_binaryOperationWithEmptyCommutative = commutative (<>) mempty

prop_currentFilledLessInited :: (Hashable a, Eq b) => TestSepChainHashMap a b -> NonEmptyList (a, b) -> Bool
prop_currentFilledLessInited hM elems =
    snd $
        foldl
            ( \acc e ->
                let newMap = uncurry (addElem (fst acc)) e
                    checkFilled = getCurrentFilled newMap < filledHashMap newMap
                 in (newMap, snd acc && checkFilled)
            )
            (unTestSCHM hM, True)
            (getNonEmpty elems)

return []
runPropertyBasedTest :: IO Bool
runPropertyBasedTest = $quickCheckAll
