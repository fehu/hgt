{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- A scope represens a system that can be safely assumed _isolated_.
-- The assumption is based on the fact that the difference of the effects
--    of distant objects on those in the system are negligible.
-- Therefore the objects within such a system (with an appropriate
--    coordinate system) are affected only by themselves.
-----------------------------------------------------------------------------

module Universe.Scope (

  Scope(..)
, CanBeScope(..)

, Interraction, CalculatedInterraction
, ObjectEffects

) where

import Measures
import Universe
import Utils


class (Scope d scope, Universe u) => CanBeScope d u scope sys where
    isIsolated :: u -> sys -> Bool
    asScope    :: u -> sys -> Maybe scope

type CalculatedInterraction d timeLapse = timeLapse -> ObjectEffects d

type Interraction d timeLapse obj = obj -> obj -> CalculatedInterraction d timeLapse

instance (Num d) => Num (CalculatedInterraction d timeLapse)
    where a + b = \t -> a t + b t


type ObjectEffects d = ( MeasuredVal d (D'  Distance) -- for impulse
                       , MeasuredVal d (D'' Distance) -- for force
                       )

instance (Num d) => Num (ObjectEffects d)
    where (a', a'') + (b', b'') = (a' + b', a'' + b'')

--zeroEffectCInteraction :: (HasZero d) => CalculatedInterraction d timeLapse
zeroEffectCInteraction _ = (MeasuredVal zero $ d' Distance, MeasuredVal zero $ d'' Distance)

class (HasZero d) => Scope d scope where
    objectsInScope :: scope -> [obj]
    objectsInterractions :: [Interraction d timeLapse obj] -> Interraction d timeLapse obj

    objectsInterractions interractions = \x y -> foldr (f x y) zeroEffectCInteraction interractions
                                      where f x y interr acc = acc + interr x y
