{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
module PropertyBasedTest (
    propertyBasedTests,
) where

import Control.Monad
import Lib
import Test.HUnit

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
                    (createHashMap 0.8 [('a', 1 :: Int)])
                    ['b' .. 'z']
            assertEqual "result size" 26 (getSize result)
        ]
