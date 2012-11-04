import System
import Rhs

instance RhsC Int where
  (-:) = (-)

s0 = S{
  sA = [[True, True, True],
        [True, True, False],
        [True, True, False]
        ],
  sB = [7,5,5],
  sC = 3,
  sR = 3,
  sP = [0..2],
  sN = 0,
  sS = Nothing
  }

s1 = addRow s0 [False,True,False] 2 

  
s2 = solve s1 :: System Int

