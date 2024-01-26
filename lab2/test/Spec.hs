import PropertyBasedTest
import Test.Tasty
import UnitTest

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyBasedTests]

main :: IO ()
main = defaultMain tests
