module UnitTest (
    unitTests,
) where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

testStringPars, testAnyChar, unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [ testStringPars
        , testAnyChar
        ]
testAnyChar =
    testGroup
        "Test parse any char"
        [testCase "one char in string" $ resultParse (parse anyChar "a") @?= 'a']
testStringPars =
    testGroup
        "Test stringPars"
        [ testCase "string are equal success" $ do
            let input = "abc"
            let parsed = parse (stringPars input) input
            resultParse parsed == input @? ("parse result wrong: " ++ resultParse parsed)
            content parsed == "" @? ("content wrong: " ++ content parsed)
        , testCase "token is prefix by input success" $ do
            let input = "abcd"
            let token = "abc"
            let parsed = parse (stringPars token) input
            resultParse parsed == token @? ("parse result worng: " ++ resultParse parsed)
            content parsed == "d" @? ("content wrong: " ++ content parsed)
        , testCase "input is prefix by token fail" $ do
            let input = "abc"
            let token = "abcd"
            let parsed = parse (stringPars token) input
            errMsg parsed /= "" @? ("error message wrong: " ++ errMsg parsed)
        ]
