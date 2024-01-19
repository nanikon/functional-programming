import PropertyBasedTest
import Test.HUnit
import UnitTest

tests :: Test
tests =
    TestList
        [ TestLabel "Unit test" unitTests
        , TestLabel "PropertyBased test" propertyBasedTests
        ]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
