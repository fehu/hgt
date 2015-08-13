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

fun2 op a b t = a t `op` b t

instance (Num d) => Num (CalculatedInterraction d timeLapse) where
    (+) = fun2 (+)
    (*) = fun2 (*)
    (-) = fun2 (-)
    abs = (.) abs
    signum = (.) signum
    fromInteger = const . fromInteger -- ??
-- from http://anton-k.github.io/ru-haskell-book/book/5.html#функциональный-калькулятор


type ObjectEffects d = ( MeasuredVal d (D'  Distance) -- for impulse
                       , MeasuredVal d (D'' Distance) -- for force
                       )

fun1p f f' (a, a') = (f a, f' a')
fun2p op op' (a', a'') (b', b'') = (a' `op` b', a'' `op'` b'')

instance (Num d) => Num (ObjectEffects d) where
    (+) = fun2p (+) (+)
    (*) = fun2p (*) (*)
    (-) = fun2p (-) (-)
    abs = fun1p abs abs
    signum = fun1p signum signum
    negate = fun1p negate negate
    fromInteger x = (MeasuredVal (fromInteger x) (d'  Distance), MeasuredVal (fromInteger x) (d''  Distance)) -- ??


--zeroEffectCInteraction :: (HasZero d) => CalculatedInterraction d timeLapse
zeroEffectCInteraction _ = (MeasuredVal zero $ d' Distance, MeasuredVal zero $ d'' Distance)

class (Num d, HasZero d) => Scope d scope where
    objectsInScope :: scope -> [obj]
    objectsInterractions :: [Interraction d timeLapse obj] -> Interraction d timeLapse obj

    objectsInterractions interractions = \x y -> foldr (f x y) zeroEffectCInteraction interractions
                                      where f x y interr acc = acc + interr x y
