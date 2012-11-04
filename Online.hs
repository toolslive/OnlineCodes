module Online(
  Online(..),
  makeOnline,
  onlineBs,
  auxIndices,
  selectIndices,
  n
             ) 
  where

import Data.Bits (shiftR)
import Prng

data Online = Online {eps :: Double
                     , onlineQ :: Int 
                     , onlineF :: Int
                     , onlineN :: Int
                     , onlineNAux :: Int
                     , onlineM :: Int      -- n + nAux
                     , onlineP :: [Double] 
                     } deriving(Show)


n :: Int
n = 64

onlineBs :: Online -> Int -> Int
onlineBs o s = (s + n - 1) `div` n where n = onlineN o

rho :: Int -> Double -> Int -> Double
rho f eps 0 = 0.0 -- for completeness
rho f eps 1 = 1.0 - ((1.0 + finv)/ (1 + eps)) where finv = 1.0 / fromIntegral f
rho f eps i = 
  let 
    rho1 = rho f eps 1 
    rho_i1 = rho f eps (i -1)
    ff = fromIntegral f
    d = fromIntegral ((f - 1) * i * (i-1))
  in
   rho_i1 + (1.0 - rho1) * (ff / d) 
    
                                                            
makeOnline :: Int -> Online
makeOnline n = Online eps q f n nAux m p
     where 
       eps = 0.01              
       q = 3
       f = floor (fn / fd)
         where 
           fn = log (eps * eps * 0.25)  
           fd = log (1   - eps * 0.5)
       nAux = max q na0
         where
           fq = fromIntegral q 
           fn = fromIntegral n
           na0 = floor (0.55 * fq * eps * fn)
       m = n + nAux
       p = map (rho f eps) [0..f] 
        
_find :: [Double] -> Int -> Int -> Double -> Int
_find xs lo hi x | lo < hi = 
    let mid = (lo + hi) `shiftR` 1 
        xMid = xs !! mid 
    in
     if x < xMid 
     then _find xs lo      mid x 
     else _find xs (mid+1) hi  x

_find xs lo hi x = lo

degree :: Online -> Seed -> (Int, Random)
degree o s = 
  (d , r')
  where 
    p = onlineP o
    pLen = length p
    r = makeRandom s
    (x,r') = nextDouble r
    d = _find p 0 pLen x
          

pickOther :: [Int] -> Random -> Int -> ([Int], Random)
pickOther es r m = --pick x < m not in es and attach to es
  let (e,r') = range r m in
  if e `elem` es 
  then pickOther es r' m
  else (e : es, r')
    
selectIndices :: Online -> Seed -> [Int]
selectIndices o s = reverse indices
  where
    m = onlineM o
    (d,r) = degree o s 
    -- in small systems, the degree can be larger than m
    -- so we need to make sure we can build the set in those cases too.
    -- hence:
    d' = min d m 
    add_one (indices, r0) i = pickOther indices r0 (onlineM o)
    (indices,_) = foldl add_one ([], r) [0..(d'-1)] 
                  
selectAux :: Online -> Int -> Random -> ([Int], Random)
selectAux o _ r0 = (reverse aux, rn)
  where 
    add_one (auxs, r) i = pickOther auxs r (onlineNAux o)
    (aux, rn) = foldl add_one ([], r0) [0..(onlineQ o - 1)]
    
auxIndices :: Online -> Random -> ([[Int]], Random) -- N lists size q
auxIndices o r = (reverse auxs, rn)
  where
    (auxs,rn) =  foldl add_one ([], r) [0..(onlineN o - 1)]
    add_one (xss,ri) i = (xss',ri')
                         where
                           (xs, ri') = selectAux o i ri
                           xss' = xs : xss 
