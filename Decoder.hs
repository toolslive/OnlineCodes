module 
  Decoder (
    makeDecoder,
    receive,
    decode,
    isDone,
    getData,
          ) 
  where


import MB
import Online
import Prng
import qualified System



data Decoder = Decoder {
  decoderS :: System.System MB, 
  decoderO :: Online} 
               

decoderN :: Decoder -> Int
decoderN d = onlineN (decoderO d)


instance Show Decoder where
  show dec = "Decoder{" ++ show (decoderS dec) ++ "}"
  

set :: [a] -> Int -> a -> [a]
set as i a = map (\ (ak,k) -> if k == i then a else ak) (zip as [0..])
  
add2List :: [[a]] -> Int -> a -> [[a]]
add2List ass i a = set ass i (a : as) where as = ass !! i
                                            
toRow :: [Int] -> Int -> [Bool] 
toRow set n = map (\i -> elem i set) [0..n]
  
makeAuxSets :: [[Int]] -> Int -> [[Int]]
makeAuxSets aIs n = 
  foldl (\ ass (mis, i) -> addQs ass (mis,i)) (replicate n []) z
    where
      addQs ass (qs,i) = foldl (\ ass q -> add2List ass q i) ass qs
      z = zip aIs [0..]

    
addAuxEquations :: System.System MB -> Online -> System.System MB
addAuxEquations sol0 onl = 
  foldl (\ sol row -> System.addRow sol row rhs0) sol0 auxRows
  where 
    nC = onlineM onl
    rhs0 = zeroMB (onlineM onl)
    r0 = makeRandom 42
    (auxIs, r1) = auxIndices onl r0 -- [[49,2,12],[54,50,17], ... ]]
    nAux = onlineNAux onl
    auxSets =   makeAuxSets auxIs nAux
    auxRows = map (\ set -> toRow set nC) auxSets



makeDecoder :: () -> Decoder
makeDecoder size = 
  Decoder { decoderS = system, decoderO = online}
  where
    online = makeOnline Online.n
    nAux = onlineNAux online
    nColumns = onlineM online
    bs = onlineBs online
    system0 = System.makeSystem nColumns
    system = addAuxEquations system0 online

receive :: Decoder -> Int -> MB -> Decoder
receive dec seed rhs = dec { decoderS = sys'}
  where 
    sys = decoderS dec
    sys' = System.addRow sys indicesAsRow rhs
    c = System.sC sys
    indices = selectIndices (decoderO dec) seed
    indicesAsRow = map (\ i -> elem i indices) [0..c]





decode :: Decoder -> Decoder
decode dec = dec { decoderS = System.solve (decoderS dec) }

isDone dec = case System.sS (decoderS dec) of
  Nothing -> False
  _ -> True



getData :: Decoder -> Maybe String
getData dec = 
  case System.sS (decoderS dec) of
    Nothing -> Nothing
    Just mbs -> Just (combineMBs mbs (decoderN dec))
