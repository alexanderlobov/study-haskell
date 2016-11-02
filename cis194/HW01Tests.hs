-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

-- testLastDigit :: (Integer, Integer) -> Bool
-- testLastDigit (n, d) = lastDigit n == d

-- testDropLastDigit :: (Integer, Integer) -> Bool
-- testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ testF1 "lastDigit test" lastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , testF1 "dropLastDigit test" dropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "toRevDigits test" toRevDigits
             [(1234, [4, 3, 2, 1]), (0, []), (-17, []), (1, [1])]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "doubleEveryOther test" doubleEveryOther
             [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0,0]), ([1], [1])]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF1 "sumDigits test" sumDigits
             [([10, 5, 18, 4], 19), ([0], 0), ([1], 1), ([1,2,3], 6),
              ([12, 13], 7)]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF1 "luhn test" luhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "hanoi test" hanoiDefault
             [ (0, [])
             , (1, [("a", "b")])
             , (2, [("a", "c"), ("a", "b"), ("c", "b")])
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
