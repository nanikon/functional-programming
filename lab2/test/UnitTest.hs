module UnitTest (
    unitTests
) where

import Lib 
import Test.HUnit

testCreate, unitTests :: Test
unitTests = TestList [TestLabel "test createHashMap" testCreate]

testCreate = 
    TestList 
        [ "current filled less then inited" ~: True ~=? 0.8 >= getCurrentFilled (createHashMap 0.8 [('b', "a"), ('a', "a")]) 
        ]