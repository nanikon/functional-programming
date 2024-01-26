import PropertyBasedTest
import System.Exit
import Test.Tasty
import Test.Tasty.HUnit
import UnitTest

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyBasedTests]

main :: IO ()
main = defaultMain tests
