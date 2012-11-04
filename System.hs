module System(
  System(..),
  makeSystem,
  addRow,
  solve
             )
  where

import Rhs
import Data.List (sortBy)

type Row = [Bool]

get row i = row !! i
set row i e = zipWith (\ rj j ->if j == i then e else rj) row [0..]

degree row = foldl (\ acc e -> if e then acc + 1 else acc) 0 row
  
sub rowa rowb k =
  zipWith3 subOne rowa rowb [0..]
  where
    xor True  True  = False
    xor False False = False
    xor _     _     = True
    
    subOne aj bj j =
      if j < k 
      then False
      else xor aj bj
      
data {- (RhsC rhs) => -} System rhs = S {
  sA :: [Row],      -- matrix A
  sB :: [rhs],      -- vector B (right hand side)
  sC :: Int,        -- number of columns (unknowns)
  sR :: Int,        -- number of rows
  sP :: [Int],      -- permutation, tracks swapping of rows
  sN :: Int,        -- index of next column that needs to be used
  sS :: Maybe [rhs] -- solution to the problem, if any
  } deriving(Show)
         
makeSystem :: Int -> System rhs
makeSystem nC = S { 
  sA = [],
  sB = [],
  sC = nC,
  sR = 0,
  sP = [],
  sN = 0,
  sS = Nothing
  }
                
         

findBestPivot s i =  
  loop Nothing i
  where
    a = sA s
    loop best j =
      if j == (sR s)
      then best
      else loop best' (j+1)
        where
          aj = a !! j
          best' =
            if get aj i 
            then
              let d = degree aj in
              case best of
                Just (bj,bd) | bd <= d -> best
                _                      -> Just (j,d)
            else
              best

swap a i j = 
  loop [] 0 a
  where
    ai = a !! i
    aj = a !! j
    loop acc k []     = reverse acc
    loop acc k (ak:as) = loop acc' (k+1) as
      where
        e = if k == i then aj
            else if k == j then ai
                 else ak
        acc' = e : acc
        
        
swapRows :: System rhs -> Int -> Int -> System rhs
swapRows s i j =
  s {
    sA = swap (sA s) i j,
    sB = swap (sB s) i j,
    sP = swap (sP s) i j 
    }
    

setRow :: System rhs -> Int -> Row -> rhs -> System rhs
setRow s i ri bi =
  s {
    sA = set (sA s) i ri,
    sB = set (sB s) i bi
    }
         


makeZeroes :: RhsC rhs => System rhs -> Int -> System rhs -- zero columns under i using i
makeZeroes s i =
  loop s (i+1)
  where 
    ai = get (sA s) i
    bi = get (sB s) i
    r = sR s
    loop s j =
      if j == r 
      then s { sN = i + 1}
      else
        loop s' (j+1)
        where
          aj' = sub aj ai i
          bj  = get (sB s) j
          bj' = bj -: bi 
          aj = get (sA s) j
          s' = if get aj i
               then setRow s j aj' bj'
               else s

data SS rhs = 
  Ok (System rhs)
  | Stuck (System rhs) Int
    deriving(Show)

cols (Ok s) = (sC s)
cols (Stuck s _) = (sC s)

reduceBelow s i = 
  case findBestPivot s i of
    Nothing -> Stuck s i
    Just(p,d) -> Ok (makeZeroes s' i)
      where s' = if p ==i then s else (swapRows s i p)
          

toR :: RhsC rhs => SS rhs -> SS rhs
toR ss = -- transform s into an equivalent right triangle matrix
  loop ss 0 
    where
      loop ss i | i == (cols ss) = ss
      loop (Ok s) i = loop (reduceBelow s i) (i+1)
      loop ss _ = ss

backSubstitute :: RhsC rhs => System rhs -> [rhs]
backSubstitute s =
  loop [] (sC s - 1)
  where
    loop xs j | j < 0 = xs
    loop xs j         = loop xs' (j-1)
      where 
        aj = get (sA s) j
        bj = get (sB s) j
        xj = foldl (\ acc (xk,ak) -> if ak then acc -: xk else acc) bj (zip xs (drop (j+1) aj))
        xs' = xj : xs
      
unpermute ps xs =
  map fst sorted
  where 
    c (x0,p0) (x1,p1) = compare p0 p1
    sorted = sortBy c (zip xs ps)

solve :: RhsC rhs => System rhs -> System rhs
solve s = 
  case toR (Ok s) of
    Ok s'     -> s' { sS = Just (unpermute (sP s) (backSubstitute s')) }
    Stuck s i -> s 
      
  
addRow :: System rhs -> Row -> rhs -> System rhs
addRow s r b = s { 
  sA = (sA s) ++ [r],
  sB = (sB s) ++ [b],
  sR = (sR s) + 1,
  sP = (sP s) ++ [(sR s)],
  sN = 0}
                   


