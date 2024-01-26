module UnitTest (
    unitTests,
) where

import Data.Char
import Lib
import Test.Tasty
import Test.Tasty.HUnit

testCreate, testGet, testDelete, testAdd, testFilter, testMap, testFold, unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [ testCreate
        , testGet
        , testDelete
        , testAdd
        , testFilter
        , testMap
        , testFold
        ]
testCreate =
    testGroup
        "Test createHashMap"
        [ testCase "current filled less then inited" $ do
            let initedFill = 0.8
            let hM = createHashMap initedFill [('b', "a"), ('a', "a")]
            getCurrentFilled hM <= initedFill @? "Current fill more than inited"
        , testCase "remove dublicates key" $ getSizeMap (createHashMap 0.8 [('a', '1'), ('a', '2')]) @?= 1
        , testCase "don't remove dublicates hash" $ getSizeMap (createHashMap 0.8 [('a', '1'), ('f', '2')]) @?= 2
        , testCase "two elem with same hash key not change fill" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let diffHashHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getCurrentFilled sameHashHM < getCurrentFilled diffHashHM @? "Not less filled where less buckets use"
        ]
testGet =
    testGroup
        "Test getElem"
        [ testCase "can get elem with exists key" $ do
            let diffHashHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getElem diffHashHM 'a' @?= Just '1'
        , testCase "can get correct elem then two key has same hash" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            getElem sameHashHM 'a' @?= Just '1'
            getElem sameHashHM 'f' @?= Just '2'
        , testCase "can't get elem with not exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getElem hM 'c' @?= Nothing
            getElem hM 'f' @?= Nothing
        ]
testDelete =
    testGroup
        "Test deleteElem"
        [ testCase "can delete elem with exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let deletedKey = 'a'
            let deletedHM = deleteElem hM deletedKey
            getSizeMap deletedHM < getSizeMap hM @? "Not change size"
            getElem deletedHM deletedKey @?= Nothing
        , testCase "can delete correct elem then two key has same hash" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let deletedKey = 'a'
            getElem (deleteElem sameHashHM deletedKey) deletedKey @?= Nothing
        , testCase "map not changed when delete elem with not exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let deletedHM = deleteElem hM 'b'
            deletedHM @?= hM
        , testCase "map not changed when delete elem with not exists key but exists key hash" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let deletedHM = deleteElem hM 'f'
            deletedHM @?= hM
        ]
testAdd =
    testGroup
        "Test addElem"
        [ testCase "can add elem with new key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'c'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getSizeMap addedHM > getSizeMap hM @? "Not change size"
            getElem addedHM addedKey @?= Just addedValue
            getCurrentFilled addedHM > getCurrentFilled hM @? "Not change filled"
        , testCase "can add elem with new key with same hash" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'f'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getSizeMap addedHM > getSizeMap hM @? "Not change size"
            getElem addedHM addedKey @?= Just addedValue
            getCurrentFilled addedHM @?= getCurrentFilled hM
        , testCase "replace value when add elem with exist key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'c'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getElem addedHM addedKey @?= Just addedValue
        , testCase "change bucket count then filled overflow" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedHM = addElem hM 'c' '3'
            let doubleAddedHM = addElem addedHM 'd' '4'
            getCurrentFilled addedHM > getCurrentFilled doubleAddedHM @? "Not resize hashmap"
        ]
testFilter =
    testGroup
        "Test filterHashMap"
        [ testCase "filter hashMap" $ do
            let firstHM = createHashMap 0.8 [('a', '1'), ('b', '2'), ('c', '3')]
            let secondHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let filteredHM = filterHashMap (\a -> fst a < 'c') firstHM
            secondHM @?= filteredHM
        ]
testMap =
    testGroup
        "Test mapHashMap"
        [ testCase "map key to string and double, value to int" $ do
            let firstHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let secondHM = createHashMap 0.8 [("aa", 1 :: Int), ("bb", 2 :: Int)]
            let mappedHM = mapHashMap (\(a, b) -> ([a, a], ord b - ord '0')) firstHM
            secondHM @?= mappedHM
        ]
testFold =
    testGroup
        "Test foldHashMap"
        [ testCase "left fold iterate from min hash to max" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            foldlHashMap (\acc (a, b) -> a : b : acc) "" hM @?= "b2a1"
        , testCase "right fold iterate from max hash to min" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            foldrHashMap (\(a, b) acc -> a : b : acc) "" hM @?= "a1b2"
        ]
