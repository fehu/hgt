{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Universe.PhysicalLaws (

  GravityField(..)

) where

import Measures


class (Measure m) => GravityField obj m where
    gravityPotential :: obj a -> m

