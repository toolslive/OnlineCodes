module Encoder(
  Encoder,
  initEncoder,
  generate,
  makeCheckBlocks,
  ) 
       where

import Online
import Prng
import MB
import Data.Bits(xor)


data MBA = MBA [MB]

type Encoder = (Int, Online, MBA, [MB])


getA :: MBA -> Int -> MB
getA (MBA mbs) i = mbs !! i

_buildAux :: [Int] -> Int -> MBA -> [MB] -> [MB] 
_buildAux []          mbi mbs abs = abs
_buildAux (abi:abis)  mbi mbs abs = _buildAux abis mbi mbs abs'
                                    
  where
    abs' = zipWith maybeXor abs [0..]
    maybeXor ab i | i /= abi  = ab
    maybeXor ab i             = xorMB ab mb where mb = getA mbs mbi




initEncoder :: String -> Encoder
initEncoder d = 
  (bs, o, mbs, abs)
  where 
    ds = length d
    o = makeOnline (Online.n)
    bs = onlineBs o ds
    mbs = MBA (splitInMBs bs d)
    abs0 = [ zeroMB bs | _ <- [0..(onlineNAux o - 1)]]
    (abis,_) = auxIndices o (makeRandom 42)
    abs = fst $ foldl do_one (abs0,0) abis
    do_one (abs, mbi) abis = (_buildAux abis mbi mbs abs, mbi+1) 

  
generate :: Encoder -> Seed -> MB 
generate (bs, o, mbs,abs)  seed = 
  foldl xorOne g indices
  where 
      g = zeroMB bs
      indices = selectIndices o seed
      n = onlineN o
      xorOne xxx i | i < n = xorMB xxx (getA mbs i)
      xorOne xxx i         = xorMB xxx ((!!) abs (i- n))

makeCheckBlocks :: Encoder -> Int -> Seed -> [(Int, Seed,MB)]
makeCheckBlocks ec nc s0 = mbs 
  where
    (r,mbs) = foldl generate_one (r0,[]) [0..nc - 1]
      where
        r0 = makeRandom s0
        generate_one (r,acc) i = 
          let 
            (si,r') = range r (randomM r) 
            seed = si `xor` 0xf0f0f0 
            mb = generate ec seed 
          in
           (r', (i, seed, mb) : acc)