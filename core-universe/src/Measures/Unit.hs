{-# LANGUAGE ExistentialQuantification
           , TypeOperators
           , DeriveDataTypeable
           #-}


module Measures.Unit (
  Unit(..)
, AtomicUnit
, CompositeUnit(..)

, UnitAtom(..)
, UnitDecomposition

, (:*)(..)
, (:/)(..)
, (:^)(..)

, D' , d'
, D'', d''

-- Atomic Units
, Time(..)
, Distance(..)
, Mass(..)
, Temperature(..)
, Luminosity(..)
, Angle(..)

-- Composite Units
, Speed(..)
, Acceleration(..)
, Force(..)
, Impulse(..)
, Energy(..)

) where

import Data.Typeable (Typeable, typeOf)

import Measures.IntType

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type UnitDecomposition = [(UnitAtom, Int)]

data UnitAtom = forall atom. (AtomicUnit atom, Typeable atom, Show atom) =>
     UnitAtom atom deriving Typeable

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Show u, Typeable u) => AtomicUnit u

instance Show UnitAtom where
    show (UnitAtom atom) = show atom

instance Eq UnitAtom where
    (UnitAtom a) == (UnitAtom b) = typeOf a == typeOf b

instance Ord UnitAtom where
    (UnitAtom a) `compare` (UnitAtom b) = show a `compare` show b

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class CompositeUnit u where
    unitDecompositionC :: u -> UnitDecomposition

data a :* b   = (:*) a b   deriving Show
data a :/ b   = (:/) a b   deriving Show
data a :^ pow = (:^) a pow deriving Show

infixl 7 :*
infixl 7 :/
infixr 8 :^
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Unit u where
    unitDecomposition :: u -> UnitDecomposition

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Time        = Time         deriving (Show, Typeable)
data Distance    = Distance     deriving (Show, Typeable)
data Mass        = Mass         deriving (Show, Typeable)
data Temperature = Temperature  deriving (Show, Typeable)
data Luminosity  = Luminosity   deriving (Show, Typeable)
data Angle       = Angle        deriving (Show, Typeable)


instance AtomicUnit Time
instance AtomicUnit Distance
instance AtomicUnit Mass
instance AtomicUnit Temperature
instance AtomicUnit Luminosity
instance AtomicUnit Angle

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type D'  a = a :/ Time
type D'' a = a :/ Time:^I2

d' :: (Unit u) => u -> D' u
d' u = u :/ Time

d'' :: (Unit u) => u -> D'' u
d'' u = u :/ Time:^I2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Speed        = Speed
data Acceleration = Acceleration
data Force        = Force
data Impulse      = Impulse
data Energy       = Energy

