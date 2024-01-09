import Test.HUnit
import UnitTest

tests :: Test
tests = TestList [TestLabel "Unit test" unitTests]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
