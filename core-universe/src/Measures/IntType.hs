module Measures.IntType (

  IntType (intValue)

, I1(..)
, I2(..)
, I3(..)
, I4(..)

) where

class IntType i where
    intValue :: i -> Int

data I1 = I1 deriving Show
data I2 = I2 deriving Show
data I3 = I3 deriving Show
data I4 = I4 deriving Show

instance IntType I1 where intValue _ = 1
instance IntType I2 where intValue _ = 2
instance IntType I3 where intValue _ = 3
instance IntType I4 where intValue _ = 4
