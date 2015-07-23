{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses
           , FunctionalDependencies
           , DeriveDataTypeable
           , ScopedTypeVariables
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
--            , EmptyDataDecls
           , TypeOperators
           #-}

module Measures2(

  Unit(..)
, UnitDecomposition

, Time(..)
, Distance(..)

) where
  
import Data.Typeable (Typeable, typeOf)
import Data.Function (on)
import Data.List     (groupBy, sortBy)
import Control.Monad (mzero)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

scalaGroupBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

class (Show u, Typeable u) => AtomicUnit u

data UnitAtom = forall atom. (AtomicUnit atom, Typeable atom, Show atom) =>
     UnitAtom atom deriving Typeable

type UnitDecomposition = [(UnitAtom, Int)]

instance Show UnitAtom where
    show (UnitAtom atom) = show atom

instance Eq UnitAtom where
    (UnitAtom a) == (UnitAtom b) = typeOf a == typeOf b

instance Ord UnitAtom where
    (UnitAtom a) `compare` (UnitAtom b) = show a `compare` show b

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

class CompositeUnit u where
    unitDecompositionC :: u -> UnitDecomposition

-- data Mult a b = Mult a b deriving Show
-- data Div  a b = Div  a b deriving Show

data a :* b = (:*) a b   deriving Show
data a :/ b = (:/) a b   deriving Show

joinUnitDecompositions :: UnitDecomposition -> UnitDecomposition -> UnitDecomposition
joinUnitDecompositions a b = let decompositions = (a ++ b) :: UnitDecomposition
                                 grouped = (scalaGroupBy fst decompositions) :: [UnitDecomposition]
                             in do (group :: UnitDecomposition) <- grouped
                                   let atom  = (fst . head $ group) :: UnitAtom
                                   let power = (sum $ map snd group) :: Int
                                   if power /= 0 then return (atom, power)
                                                 else mzero -- []

instance (Unit' ua a, UnitDecompositionEvidence a ua, Unit' ub b, UnitDecompositionEvidence b ub) => CompositeUnit (a :* b) where
    unitDecompositionC (x :* y) = joinUnitDecompositions (unitDecomposition x) (unitDecomposition y)

instance (Unit' ua a, UnitDecompositionEvidence a ua, Unit' ub b, UnitDecompositionEvidence b ub) => CompositeUnit (a :/ b) where
    unitDecompositionC (x :/ y) = joinUnitDecompositions (unitDecomposition x) (map (\(u, p) -> (u, -p) ) $ unitDecomposition y)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

class Unit u where
    unitDecomposition :: u -> UnitDecomposition

class Unit' underlying u where
    unitDecomposition' :: underlying -> u -> UnitDecomposition
    
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

data UAtom
data UComposite

instance AtomicUnit u => Unit' UAtom u where
     unitDecomposition' _ atom = [(UnitAtom atom, 1)]

instance CompositeUnit u => Unit' UComposite u where
    unitDecomposition' _ unit = unitDecompositionC unit

class UnitDecompositionEvidence u underlying | u -> underlying

instance (UnitDecompositionEvidence u underlying, Unit' underlying u) => Unit u where
    unitDecomposition = unitDecomposition' (undefined :: underlying)


-- instance AtomicUnit u     => UnitDecompositionEvidence u UAtom

instance (Unit a, Unit b) => UnitDecompositionEvidence (a :* b) UComposite
instance (Unit a, Unit b) => UnitDecompositionEvidence (a :/ b) UComposite


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

data Time     = Time     deriving (Show, Typeable)
data Distance = Distance deriving (Show, Typeable)

instance AtomicUnit Time
instance AtomicUnit Distance

-- -- -- -- -- -- -- -- -- 

instance UnitDecompositionEvidence Time     UAtom
instance UnitDecompositionEvidence Distance UAtom
