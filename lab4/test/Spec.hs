import Test.Tasty
import UnitTest

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
