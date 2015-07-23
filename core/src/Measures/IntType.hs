module Measures.IntType (

  IntType (intValue)

, I1(..)
, I2(..)

) where

class IntType i where
    intValue :: i -> Int

data I1 = I1 deriving Show
data I2 = I2 deriving Show

instance IntType I1 where intValue _ = 1
instance IntType I2 where intValue _ = 2
