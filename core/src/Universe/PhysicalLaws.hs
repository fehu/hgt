{-# LANGUAGE MultiParamTypeClasses
            , FlexibleContexts
--           , FlexibleInstances
           #-}

module Universe.PhysicalLaws (

  GravityField(..)

) where

import Measures

--class (Measure m) => GravityField obj m where
--    gravityPotential :: obj a -> m


class (Measured m d Force) => GravityField obj d m where
    gravityPotential :: obj a -> obj b -> m



