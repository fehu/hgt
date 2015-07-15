{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-} -- RankNTypes

module Measures (
  Measure(..)
, MeasureSystem(..)

, Unit(..)

, Time(..)
, Distance(..)
, Mass(..)
, Luminosity(..)
, Temperature(..)
, Energy(..)
, Angle(..)

, Force

, Mult
, Div
, Pow2, Pow3

, D', D''
, d', d''


, Measured(..)
, MeasureConversion(..)
, MeasuredVal(..)

) where

import Data.Map (Map, fromList, keysSet, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (toList, union)
import Data.Typeable


data MeasureSystem = SI
                   | LargeScale
                   | Custom

class Measure m where
    measureName   :: m -> String
    measureSystem :: m -> MeasureSystem

class (Show m, Ord m) => Unit m where
    decomposeUnit :: m -> UnitDecomposition -- (AtomicUnit atom) =>

data AtomHolder = forall atom. (Typeable atom, AtomicUnit atom, Ord atom) => AtomHolder atom

instance Eq AtomHolder where
    (AtomHolder a) == (AtomHolder b) = compareAtoms a b
            where compareAtoms x y = typeOf x == typeOf y

--    ((cast a) :: Maybe atom) == ((cast b) :: Maybe atom)

instance Ord AtomHolder where

data UnitDecomposition = UnitDecomposition { extractDecomposition :: Map AtomHolder Int }

--type AnyUnit = exists unit. Unit unit => unit

--- -- Atomic Measures -- ---
class AtomicUnit m

data Time        = Time         deriving (Show, Eq)
data Distance    = Distance     deriving (Show, Eq)
data Mass        = Mass         deriving (Show, Eq)
data Luminosity  = Luminosity   deriving (Show, Eq)
data Temperature = Temperature  deriving (Show, Eq)
data Energy      = Energy       deriving (Show, Eq)
data Angle       = Angle        deriving (Show, Eq)


--instance Unit Time
--instance Unit Distance
--instance Unit Mass
--instance Unit Luminosity
--instance Unit Temperature
--instance Unit Energy
--instance Unit Angle


instance AtomicUnit Time
instance AtomicUnit Distance
instance AtomicUnit Mass
instance AtomicUnit Luminosity
instance AtomicUnit Temperature
instance AtomicUnit Energy
instance AtomicUnit Angle

--instance (Show m, Ord m, AtomicUnit m, Typeable m) => Unit m where
--    decomposeUnit m = UnitDecomposition $ fromList [(AtomHolder m, 1)]

instance (Show m, Ord m, AtomicUnit m, Typeable m) => Unit m where
    decomposeUnit m = UnitDecomposition $ fromList [(AtomHolder m, 1)]

--- -- Measures Composition -- ---
class ComposedUnit m where
    decomposeUnit' :: m -> UnitDecomposition

data Mult a b = Mult a b
data Div  a b = Div  a b
data Pow2 a   = Pow2 a
data Pow3 a   = Pow3 a

type D'  a = Div a Time
type D'' a = Div a (Pow2 Time)

d' :: a -> D' a
d' a = Div a Time

d'' :: a -> D'' a
d'' a = Div a (Pow2 Time)


sumMaps :: (Ord a, Num b) => Map a b -> Map a b -> Map a b
sumMaps m1 m2 = fromList $ do key <- toList keys
                              let find inMap = fromMaybe 0 $ Data.Map.lookup key inMap
                              let v1 = find m1
                              let v2 = find m2
                              return (key, v1 + v2)
             where keys = keysSet m1 `union` keysSet m2


instance (Unit a, Unit b) => ComposedUnit (Mult a b) where
    decomposeUnit' (Mult a b) = UnitDecomposition $ (decomposedUnit a) `sumMaps` (decomposedUnit b)

decomposedUnit :: (Unit u) => u -> Map AtomHolder Int
decomposedUnit x = g . decomposeUnit $ x
    where g UnitDecomposition { extractDecomposition = mp } = mp

instance (Show m, Ord m, ComposedUnit m, Typeable m) => Unit m where
    decomposeUnit = decomposeUnit'




instance (Show a, Show b) => Show (Mult a b) where
    show (Mult a b) = show a ++ "*" ++ show b

instance (Show a, Show b) => Show (Div a b) where
    show (Div a b) = show a ++ "/" ++ show b

instance (Show a) => Show (Pow2 a) where
    show (Pow2 a) = "(" ++ show a ++ ")^2"

--decomposeMeasure :: (AtomicUnit atom)

--- -- Composite Measures -- ---
type Force = D'' (Mult Mass Distance)


class (Measure m) => Measured a d m where
    measuredValue :: a -> d
    measure       :: a -> m

class (Measured c d from, Measured c d to) => MeasureConversion c d from to where
    convert ::  c -> c

data MeasuredVal d m = MeasuredVal d m -- (Measure m) =>

instance (Measure m) =>  Measured (MeasuredVal d m) d m where
    measuredValue (MeasuredVal d _) = d
    measure       (MeasuredVal _ m) = m

instance (Eq m, Show m, Num d) => Num (MeasuredVal d m) where
    (MeasuredVal d1 m1) + (MeasuredVal d2 m2) = if m1 == m2
                                                then MeasuredVal (d1 + d2) m1
                                                else error $ "different measures: "
                                                        ++ show m1 ++ ", " ++ show m2





