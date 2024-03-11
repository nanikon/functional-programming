module UnitTest (
    unitTests,
) where

import LagrangInterpolation
import LinearInterpolation
import PointWindow
import Test.Tasty
import Test.Tasty.HUnit

testParse, testLinear, testLagrange, unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [ testLagrange
        , testLinear
        , testParse
        ]
testLagrange =
    testGroup
        "Lagrange method test"
        [ testCase "in x point ys must be equal" $ do
            let window = [(0.3, 0.1), (0.1, 0.0)]
            let xs = map fst window
            let ys = map snd window
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (lagrangInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        , testCase "same withalready computed values" $ do
            let window = [(6.0, 9.0), (5.0, 5.0), (2.0, 4.0)]
            let xs = [3.0, 4.0, 5.5]
            let ys = [2.5, 2.83, 6.77]
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (lagrangInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        ]
testLinear =
    testGroup
        "Linear method test"
        [ testCase "same withalready computed values" $ do
            let window = [(6.0, 9.0), (5.0, 5.0), (2.0, 4.0)]
            let xs = [3.0, 4.0, 5.5]
            let ys = [4.3333, 4.6666, 7.0]
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (linInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        ]
testParse =
    testGroup
        "Test parse point from string"
        [ testCase "parse intvalues in string" $ parsePoint "1;1" @?= (1.0, 1.0)
        , testCase "parse double values in string" $ parsePoint "2.5;0.2345" @?= (2.5, 0.2345)
        ]
