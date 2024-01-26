module UnitTest (
    unitTests,
) where

import Data.Char
import Lib
import Test.Tasty
import Test.Tasty.HUnit

hMSameHash, hMDiffHash :: SepChainHashMap Char Char
hMDiffHash = createHashMap 0.8 [('a', '1'), ('b', '2')]
hMSameHash = createHashMap 0.8 [('a', '1'), ('f', '2')]

testCreate, testGet, testDelete, testAdd, testFilter, testMap, testFold, unitTests :: TestTree
unitTests =
    testGroup "Unit tests"
        [ testCreate
        , testGet
        , testDelete
        , testAdd
        , testFilter
        , testMap
        , testFold
        ]
testCreate =
    testGroup "Test createHashMap"
        [ testCase "current filled less then inited" $ do
            let initedFill = 0.8
            let hM = createHashMap initedFill [('b', "a"), ('a', "a")]
            getCurrentFilled hM <= initedFill @? "Current fill more than inited"
        , testCase "remove dublicates key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('a', '2')]
            getSizeMap hM == 1 @? "Not remove dublicates key"
        , testCase "don't remove dublicates hash" ~: 2 ~=? getSizeMap (createHashMap 0.8 [('a', '1'), ('f', '2')])
        , testCase "two elem with same hash key not change fill" ~: 2 * getCurrentFilled (createHashMap 0.8 [('a', '1'), ('f', '2')]) ~=? getCurrentFilled (createHashMap 0.8 [('a', '1'), ('b', '2')])
        ]
testGet =
    testGroup "Test getElem"
        [ testCase "can get elem with exists key" ~: Just '1' ~=? getElem hMDiffHash 'a'
        , testCase "can get correct elem then two key has same hash" ~: True ~=? (getElem hMSameHash 'a' == Just '1') && (getElem hMSameHash 'f' == Just '2')
        , testCase "can't get elem with not exists key" ~: Nothing ~=? getElem hMDiffHash 'c'
        , testCase "can't get elem with not exists key but exists key hash" ~: Nothing ~=? getElem hMDiffHash 'f'
        ]
testDelete =
    testGroup "Test deleteElem"
        [ testCase "can delete elem with exists key" ~: 1 ~=? getSizeMap (deleteElem hMDiffHash 'a')
        , testCase "can't get elem after delete it" ~: Nothing ~=? getElem (deleteElem hMDiffHash 'a') 'a'
        , testCase "can delete correct elem then two key has same hash" ~: Nothing ~=? getElem (deleteElem hMSameHash 'a') 'a'
        , testCase "map not changed when delete elem with not exists key" ~: hMSameHash ~=? deleteElem hMSameHash 'b'
        , testCase "map not changed when delete elem with not exists key but exists key hash" ~: hMDiffHash ~=? deleteElem hMDiffHash 'f'
        ]
testAdd =
    testGroup "Test addElem"
        [ testCase "can add elem with new key" ~: getSizeMap hMDiffHash + 1 ~=? getSizeMap (addElem hMDiffHash 'c' '3')
        , testCase "can get added elem" ~: Just '3' ~=? getElem (addElem hMDiffHash 'c' '3') 'c'
        , testCase "replace value when add elem with exist key" ~: Just '3' ~=? getElem (addElem hMDiffHash 'b' '3') 'b'
        , testCase "change filled then add elem with diff hash key" ~: 2 * getCurrentFilled hMSameHash ~=? getCurrentFilled (addElem hMSameHash 'c' '3')
        , testCase "not change filled then add elem with same hash key" ~: getCurrentFilled hMDiffHash ~=? getCurrentFilled (addElem hMDiffHash 'f' '3')
        , testCase "change bucket count then filled overflow" ~: True ~=? (getCurrentFilled (addElem hMDiffHash 'c' '3') > getCurrentFilled (addElem (addElem hMDiffHash 'c' '3') 'd' '4'))
        ]
testFilter =
    testGroup "Test filterHashMap"
        [ testCase "filter hashMap" ~: hMDiffHash ~=? filterHashMap (\a -> fst a == 'a' || snd a == '2') (addElem (addElem hMDiffHash 'f' '3') 'c' '4')]
testMap =
    testGroup "Test mapHashMap"
        [ testCase "map key to string and double, value to int" ~: createHashMap 0.8 [("aa", 1 :: Int), ("bb", 2 :: Int)] ~=? mapHashMap (\(a, b) -> ([a, a], ord b - ord '0')) hMDiffHash]
testFold =
    testGroup "Test foldHashMap"
        [ testCase "left fold" ~: "b2a1" ~=? foldlHashMap (\acc (a, b) -> a : b : acc) "" hMDiffHash
        , testCase "right fold" ~: "a1b2" ~=? foldrHashMap (\(a, b) acc -> a : b : acc) "" hMDiffHash
        ]
