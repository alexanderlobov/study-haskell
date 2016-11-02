module HW02Tests where

import HW02
import Testing

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             , ([Red, Blue, Green, Yellow], [Red, Blue, Green, Yellow], 4)
             ]
           ]

ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1,0,1,1,0,1])
             , ([Green, Blue, Green, Orange], [0,2,1,0,1,0])
             , ([Red, Red, Red, Red], [4,0,0,0,0,0])
             ]
           ]

ex2Tests' = [ testF2 "matches test" matches
              [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)
              ]
            ]

ex3Tests = [ testF2 "getMove test" getMove
             [( [Red, Blue, Yellow, Orange]
              , [Red, Orange, Orange, Blue]
              , Move [Red, Orange, Orange, Blue] 1 2)]
           ]

ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ ( Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple], True)
             , ( Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple], False)
             ]
           ]

ex6Tests = [ testF1 "allCodes test" allCodes
             [ (1,
                [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]) ]
           ]
