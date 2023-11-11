import Test.HUnit
import qualified System.Exit as Exit

import Case1

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (case1 1 2))

test2 :: Test
test2 = TestCase (assertEqual "should return 3" 3 (case3 case2 2))

test3 :: Test
test3 = TestCase (assertEqual "should return 3" 1 (case3 case4 2))

test4 :: Test
test4 = TestCase (assertEqual "should return 3" 3 (case3 (case1 1) 2))
 
tests :: [Test]
tests = [ 
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4
    ]

main :: IO ()
main = do
    result <- runTestTT $ TestList tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess