
module MB(
  MB,
  xorMB,
  zeroMB,
  splitInMBs,
  combineMBs,
  ) 
       where

import Data.Bits(xor)
import Data.Char(chr,ord)

import Rhs




data MB = MB String

showMB :: MB -> String
showMB (MB mb) = 
  "< " ++ (summ mb) ++ "> " 
  where 
    summ [] = " 0"
    summ (x : r) = show x ++ " ..." ++ show (length mb)

xorMB :: MB -> MB -> MB
xorMB (MB a) (MB b) = MB (zipWith xorC a b)
  where xorC ca cb = chr ((ord ca) `xor` (ord cb))

c0 = chr 0
zeroMB :: Int -> MB
zeroMB n = MB (replicate n c0)

_rconvert :: String -> MB
_rconvert mb = MB (reverse mb)

_split :: Int -> [MB] -> String -> Int -> String -> [MB]
_split bs acc cb i  []                = reverse (rcb : acc)          where rcb = _rconvert cb
_split bs acc cb i (c : t)  | i == bs = _split bs (rcb:acc) [c] 1  t where rcb = _rconvert cb
_split bs acc cb i (c : t)            = _split bs     acc            (c:cb)  (i+1) t

splitInMBs :: Int -> String -> [MB]
splitInMBs bs = _split bs [] [] 0

combineMBs :: [MB] -> Int -> String
combineMBs mbs n = 
  x
    where 
      (x,_) = foldl (\ (acc,i) (MB mb) -> if i < n then (acc ++ mb, i+1) else (acc,i+1)) 
              ("",0) mbs

instance Show MB where
  show = showMB
  
instance RhsC MB where
  (-:) = xorMB