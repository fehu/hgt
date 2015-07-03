{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Measures (
  Measure(..)
, MeasureSystem(..)

, Measured(..)
, MeasureConversion(..)
, MeasuredVal

, Time
, Distance
, Mass
, Luminosity
, Temperature
, Energy

, Force

, Mult
, Div
, Pow2
, D', D''

) where

data MeasureSystem = SI
                   | LargeScale
                   | Custom

class Measure m where
    measureName   :: m -> String
    measureSystem :: m -> MeasureSystem

--- -- Atomic Measures -- ---
data Time        = Time
data Distance    = Distance
data Mass        = Mass
data Luminosity  = Luminosity
data Temperature = Temperature
data Energy      = Energy

--- -- Measures Composition -- ---
data Mult a b = Mult a b
data Div  a b = Div  a b
data Pow2 a   = Pow  a

type D'  a = Div a Time
type D'' a = Div a (Pow2 Time)


--- -- Composite Measures -- ---
type Force = D'' (Mult Mass Distance)




class (Measure m) => Measured a d m where
    measuredValue :: a -> d
    measure       :: a -> m

class (Measured c d from, Measured c d to) => MeasureConversion c d from to where
    convert ::  c -> c

data (Measure m) => MeasuredVal d m = MeasuredVal d m

instance (Measure m) =>  Measured (MeasuredVal d m) d m where
    measuredValue (MeasuredVal d _) = d
    measure       (MeasuredVal _ m) = m
