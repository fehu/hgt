{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
--           , UndecidableInstances
           #-}

module Measures(

  Unit(..)
, UnitDecomposition

-- Atomic Units
, Time(..)
, Distance(..)
, Mass(..)
, Temperature(..)
, Luminosity(..)
, Angle(..)
------------------

, (:*)(..)
, (:/)(..)
, (:^)(..)

, I1(..)
, I2(..)
, I3(..)
, I4(..)

, D' , d'
, D'', d''

-- Composite Units
, Speed(..)
, Acceleration(..)
, Force(..)
, Impulse(..)
, Energy(..)
------------------

, Measure(..)
, Measured(..)
, MeasuredVal(..)

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

--data DimensionlessMeasure = DimensionlessMeasure
--instance Measure DimensionlessMeasure

data MeasuredVal d m = MeasuredVal d m


instance (Measure m) => Measured (MeasuredVal d m) d m where
    measured measure value = MeasuredVal value measure
    measuredValue (MeasuredVal val _) = val
    measure (MeasuredVal _ m) = m

--instance (Measured a d m, Measure m) => Num a

instance (Num d) => Num (MeasuredVal d m) where
    (MeasuredVal x m) + (MeasuredVal y _) = MeasuredVal (x + y) m
    (MeasuredVal x m) * (MeasuredVal y _) = MeasuredVal (x * y) m
    negate (MeasuredVal x m)              = MeasuredVal (negate x) m
    abs (MeasuredVal x m)                 = MeasuredVal (abs x) m
    signum (MeasuredVal x m)              = MeasuredVal (signum x) m
    fromInteger int                       = MeasuredVal (fromInteger int) (undefined :: m)

