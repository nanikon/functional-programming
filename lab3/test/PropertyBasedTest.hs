module PropertyBasedTest (
    propertyBasedTests,
) where

import Data.List (nubBy, sortBy)
import LagrangInterpolation
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

propertyBasedTests :: TestTree
propertyBasedTests =
    testGroup
        "Property-based tests"
        [ testProperty "lagrange must be eq with y in x" (lagrangeEq :: [(Double, Double)] -> Bool)
        ]

lagrangeEq :: [(Double, Double)] -> Bool
lagrangeEq w = all (\el -> uncurry (-) el < 0.001) (zip (ys (window w)) (map (lagrangInterOnePoint (window w)) (xs (window w))))
  where
    window = sortBy (flip (\a b -> compare (fst a) (fst b))) . nubBy (\a b -> fst a == fst b)
    xs = map fst
    ys = map snd
