module UnitTest (
    unitTests,
) where

import Data.Char
import Lib
import Test.HUnit

hMSameHash, hMDiffHash :: SepChainHashMap Char Char
hMDiffHash = createHashMap 0.8 [('a', '1'), ('b', '2')]
hMSameHash = createHashMap 0.8 [('a', '1'), ('f', '2')]

testCreate, testGet, testDelete, testAdd, testFilter, testMap, unitTests :: Test
unitTests =
    TestList
        [ TestLabel "test createHashMap" testCreate
        , TestLabel "test getElem" testGet
        , TestLabel "test deleteElem" testDelete
        , TestLabel "test addElem" testAdd
        , TestLabel "test filterHashMap" testFilter
        , TestLabel "test mapHashMap" testMap
        ]
testCreate =
    TestList
        [ "current filled less then inited" ~: True ~=? 0.8 >= getCurrentFilled (createHashMap 0.8 [('b', "a"), ('a', "a")])
        , "remove dublicates key" ~: 1 ~=? getSize (createHashMap 0.8 [('a', '1'), ('a', '2')])
        , "don't remove dublicates hash" ~: 2 ~=? getSize (createHashMap 0.8 [('a', '1'), ('f', '2')])
        , "two elem with same hash key not change fill" ~: 2 * getCurrentFilled (createHashMap 0.8 [('a', '1'), ('f', '2')]) ~=? getCurrentFilled (createHashMap 0.8 [('a', '1'), ('b', '2')])
        ]
testGet =
    TestList
        [ "can get elem with exists key" ~: Just '1' ~=? getElem hMDiffHash 'a'
        , "can get correct elem then two key has same hash" ~: True ~=? (getElem hMSameHash 'a' == Just '1') && (getElem hMSameHash 'f' == Just '2')
        , "can't get elem with not exists key" ~: Nothing ~=? getElem hMDiffHash 'c'
        , "can't get elem with not exists key but exists key hash" ~: Nothing ~=? getElem hMDiffHash 'f'
        ]
testDelete =
    TestList
        [ "can delete elem with exists key" ~: 1 ~=? getSize (deleteElem hMDiffHash 'a')
        , "can't get elem after delete it" ~: Nothing ~=? getElem (deleteElem hMDiffHash 'a') 'a'
        , "can delete correct elem then two key has same hash" ~: Nothing ~=? getElem (deleteElem hMSameHash 'a') 'a'
        , "map not changed when delete elem with not exists key" ~: hMSameHash ~=? deleteElem hMSameHash 'b'
        , "map not changed when delete elem with not exists key but exists key hash" ~: hMDiffHash ~=? deleteElem hMDiffHash 'f'
        ]
testAdd =
    TestList
        [ "can add elem with new key" ~: getSize hMDiffHash + 1 ~=? getSize (addElem hMDiffHash 'c' '3')
        , "can get added elem" ~: Just '3' ~=? getElem (addElem hMDiffHash 'c' '3') 'c'
        , "replace value when add elem with exist key" ~: Just '3' ~=? getElem (addElem hMDiffHash 'b' '3') 'b'
        , "change filled then add elem with diff hash key" ~: 2 * getCurrentFilled hMSameHash ~=? getCurrentFilled (addElem hMSameHash 'c' '3')
        , "not change filled then add elem with same hash key" ~: getCurrentFilled hMDiffHash ~=? getCurrentFilled (addElem hMDiffHash 'f' '3')
        , "change bucket count then filled overflow" ~: True ~=? (getCurrentFilled (addElem hMDiffHash 'c' '3') > getCurrentFilled (addElem (addElem hMDiffHash 'c' '3') 'd' '4'))
        ]
testFilter =
    TestList
        ["filter hashMap" ~: hMDiffHash ~=? filterHashMap (\a -> fst a == 'a' || snd a == '2') (addElem (addElem hMDiffHash 'f' '3') 'c' '4')]
testMap =
    TestList
        ["map key to string and double, value to int" ~: createHashMap 0.8 [("aa", 1 :: Int), ("bb", 2 :: Int)] ~=? mapHashMap (\(a, b) -> ([a, a], ord b - ord '0')) hMDiffHash]
