module Rhs (
  RhsC,
  (-:),
           )
       
       where

class RhsC a where
  (-:) :: a -> a -> a
  