{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}

module Measures(

  Unit(..)
, UnitDecomposition

-- Atomic Units
, Time(..)
, Distance(..)
, Mass(..)
, Temperature(..)
, Angle(..)
------------------

, (:*)(..)
, (:/)(..)
, (:^)(..)

, I1(..)
, I2(..)

, D' , d'
, D'', d''

-- Composite Units
, Speed(..)
, Acceleration(..)
, Force(..)
, Energy(..)
------------------

, Measure(..)
, Measured(..)

, MeasureSystem (SI, LargeScale)
) where

import Measures.IntType
import Measures.Unit
import Measures.Unit.Internal

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data MeasureSystem = SI
                   | LargeScale
--                   | TODO


class Measure m where
    measureName   :: m -> String
    measureSystem :: m -> MeasureSystem


class (Measure m) => Measured a d m where
    measured      :: m -> d -> a
    measuredValue :: a -> d
    measure       :: a -> m


data MeasuredVal d m = MeasuredVal d m

instance (Measure m) => Measured (MeasuredVal d m) d m where
    measured measure value = MeasuredVal value measure
    measuredValue (MeasuredVal val _) = val
    measure (MeasuredVal _ m) = m

