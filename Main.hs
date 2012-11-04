
import Encoder
import Decoder
import Data.Char (chr)
import qualified Online 


makeData :: Int -> String
makeData bs = 
  let 
    makeChar i = chr ((i `div` 64) `mod` 256)
    dSize = bs * Online.n
  in
  [makeChar i | i <- [0..dSize -1]]
  
  
doOne s0 =
  putStrLn ("seed: " ++ show s0  ++ "\n" ++
            "bs: "   ++ show bs  ++ "\n" ++
            "nc: "   ++ show nc  ++ "\n" 
            -- ++ "cbs: \n"  ++ show' mbs ++ "\n"
            ++ "solved? "   ++ show (Decoder.isDone dc2)    ++ "\n"
            ++ "d0 == d1: " ++ show (ok) ++ "\n"
           )
    where
      show' mbs = foldl (++) "" (map show_e (reverse mbs))
      show_e (i, s,mb) = "[(" ++ show i ++ ", " ++ show s ++ ", " ++ show mb ++ ")]\n"
      bs = 16
      d0 = makeData bs
      ec = initEncoder d0
      nc = round  ((fromIntegral Online.n) * 1.4)
      mbs = makeCheckBlocks ec nc s0

      -- decoder:
      dc0 = makeDecoder ()
      dc1 = foldl (\dc (i,s,mb) -> receive dc s mb) dc0 mbs
      dc2 = decode dc1
      dc2D = Decoder.getData dc2
      d1L = case dc2D of
        Nothing -> -1
        Just d1 -> length d1
      ok = case dc2D of
        Nothing -> False
        Just d1 -> d1 == d0


main = 
  loop 100 
    where
      loop 0 = return ()
      loop i = doOne i >>= \ () -> loop (i-1)

  
  
