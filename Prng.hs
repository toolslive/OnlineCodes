module Prng (
  Seed,
  Random(..),
  makeRandom,
  nextDouble,
  range,
  ) where

type Seed = Int
data Random = Random { randomSeed::Seed
                     , randomA :: Int
                     , randomM ::Int
                     } deriving(Show)

makeRandom :: Seed -> Random
makeRandom s = Random s 16807 2147483647

next :: Random -> Random
next (Random s a m) = Random s' a m where s' = (a * s) `mod` m

range :: Random -> Int -> (Int, Random)
range r max = (x, r') 
  where r' = next r
        x = randomSeed r' `mod` max
        
nextDouble :: Random -> (Double,Random)
nextDouble r = (x,r') 
  where r' = next r
        mf = fromIntegral (randomM r')
        seed' = fromIntegral (randomSeed r')
        x = seed' / mf
        