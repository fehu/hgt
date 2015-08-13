{-# LANGUAGE TypeOperators
           , EmptyDataDecls
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables
           #-}

module Measures.Unit.Internal (

) where

import Measures.Unit
import Measures.IntType

import Control.Monad (mzero)
import Utils (scalaGroupBy)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data UAtom
data UComposite

class UnitDecompositionEvidence u underlying | u -> underlying

class Unit' underlying u where
    unitDecomposition' :: underlying -> u -> UnitDecomposition

instance (UnitDecompositionEvidence u underlying, Unit' underlying u) => Unit u where
    unitDecomposition = unitDecomposition' (undefined :: underlying)

instance AtomicUnit u => Unit' UAtom u where
     unitDecomposition' _ atom = [(UnitAtom atom, 1)]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

joinUnitDecompositions :: UnitDecomposition -> UnitDecomposition -> UnitDecomposition
joinUnitDecompositions a b = let grouped = scalaGroupBy fst (a ++ b)
                             in do group <- grouped
                                   let atom  = fst . head $ group
                                   let power = sum $ map snd group
                                   if power /= 0 then return (atom, power)
                                                 else mzero -- []

instance (Unit' ua a, UnitDecompositionEvidence a ua, Unit' ub b, UnitDecompositionEvidence b ub) => CompositeUnit (a :* b) where
    unitDecompositionC (x :* y) = joinUnitDecompositions (unitDecomposition x) (unitDecomposition y)

instance (Unit' ua a, UnitDecompositionEvidence a ua, Unit' ub b, UnitDecompositionEvidence b ub) => CompositeUnit (a :/ b) where
    unitDecompositionC (x :/ y) = joinUnitDecompositions (unitDecomposition x) (map (\(u, p) -> (u, -p) ) $ unitDecomposition y)

instance (Unit' ua a, UnitDecompositionEvidence a ua, IntType pow) => CompositeUnit (a :^ pow) where
    unitDecompositionC (x :^ pow) = map (\(u, p) -> (u, p * intValue pow)) $ unitDecomposition x



instance CompositeUnit u => Unit' UComposite u where
    unitDecomposition' _ = unitDecompositionC


instance (Unit a, Unit b)      => UnitDecompositionEvidence (a :* b)    UComposite
instance (Unit a, Unit b)      => UnitDecompositionEvidence (a :/ b)    UComposite
instance (Unit a, IntType pow) => UnitDecompositionEvidence (a :^ pow)  UComposite

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance UnitDecompositionEvidence Time         UAtom
instance UnitDecompositionEvidence Distance     UAtom
instance UnitDecompositionEvidence Mass         UAtom
instance UnitDecompositionEvidence Temperature  UAtom
instance UnitDecompositionEvidence Angle        UAtom

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance UnitDecompositionEvidence Speed UComposite
instance CompositeUnit             Speed where
    unitDecompositionC _ = unitDecomposition $ d' Distance

instance UnitDecompositionEvidence Acceleration UComposite
instance CompositeUnit             Acceleration where
    unitDecompositionC _ = unitDecomposition $ d'' Distance

instance UnitDecompositionEvidence Force UComposite
instance CompositeUnit             Force where
    unitDecompositionC _ = unitDecomposition $ Mass :* Acceleration

instance UnitDecompositionEvidence Impulse UComposite
instance CompositeUnit             Impulse where
    unitDecompositionC _ = unitDecomposition $ Mass :* Acceleration


instance UnitDecompositionEvidence Energy UComposite
instance CompositeUnit             Energy where
    unitDecompositionC _ = unitDecomposition $ Distance :* Force

