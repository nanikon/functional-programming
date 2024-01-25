import PropertyBasedTest
import System.Exit
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
    resultPBT <- and <$> sequence [runPropertyBasedTest]
    _ <- if resultPBT then exitSuccess else exitFailure
    return ()
