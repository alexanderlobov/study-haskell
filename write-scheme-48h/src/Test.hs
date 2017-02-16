import Scheme
import Test.HUnit

test1 = TestCase (assertEqual "first test" 1 2)
test2 = TestCase (assertEqual "second test" 1 1)
test3 = TestCase $ 1 @=? 3

tests = TestList [test1, test2, test3]

main = runTestTT tests
