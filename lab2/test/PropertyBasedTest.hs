{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
module PropertyBasedTest (
    propertyBasedTests,
    runPropertyBasedTest,
) where

import Control.Monad
import Data.Hashable
import Lib
import Test.HUnit
import Test.Invariant
import Test.QuickCheck

newtype TestSepChainHashMap a b = TSCHM (SepChainHashMap a b) deriving (Semigroup, Monoid, Eq, Show)

instance (Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (TestSepChainHashMap a b) where
    arbitrary = TSCHM <$> (createHashMap <$> filled <*> listOf arbitrary)
      where
        filled = choose (0.1, 0.9)

prop_binaryOperationAssociative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> TestSepChainHashMap a b -> TestSepChainHashMap a b -> Bool
prop_binaryOperationAssociative = associative (<>)

prop_binaryOperationWithEmptyNotChange :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
prop_binaryOperationWithEmptyNotChange x = x == (x <> mempty)

prop_binaryOperationWithEmptyCommutative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
prop_binaryOperationWithEmptyCommutative = commutative (<>) mempty

-- prop_currentFilledLessInited :: (Hashable a) => TestSepChainHashMap a b -> [NonEmptyList Elem a b] -> Bool
-- prop_currentFilledLessInited map elems =

return []
runPropertyBasedTest :: IO Bool
runPropertyBasedTest = $quickCheckAll

fistHM, secondHM, thirdHM :: SepChainHashMap Char Int
fistHM = createHashMap 0.8 [('a', 1), ('b', 1)]
secondHM = createHashMap 0.8 [('c', 2), ('d', 2)]
thirdHM = createHashMap 0.6 [('e', 3), ('f', 3)]

propertyBasedTests :: Test
propertyBasedTests =
    TestList
        [ "binary operation must be associative" ~: ((fistHM <> secondHM) <> thirdHM) ~=? (fistHM <> (secondHM <> thirdHM))
        , "binary operation with empty must not change map" ~: fistHM ~=? fistHM <> mempty
        , "bianry operation with empty must be commutative" ~: fistHM <> mempty ~=? mempty <> fistHM
        , "current filled must be less then inited" ~: do
            result <-
                foldM
                    ( \acc e -> do
                        let newMap = addElem acc e (1 :: Int)
                        assertEqual ("add elem with key" ++ show e) True (getCurrentFilled newMap < filledHashMap newMap)
                        return newMap
                    )
                    (createHashMap 0.8 [('A', 1 :: Int)])
                    ['B' .. 'z']
            assertEqual "result size" 58 (getSizeMap result)
        ]
