{-# LANGUAGE TypeOperators
           , ExistentialQuantification
           , FlexibleInstances
           , ImplicitParams
           #-}

module U (

) where



import Data.UUID
import Control.Monad (mzero)

import Utils


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



data Planet d = Planet{ planetId          :: UUID
                      , planetDescriptor  :: PlanetDescriptor d
                      }

data PlanetDescriptor d = PlanetDescriptor { planetMass                 :: d -- Mass
                                           , planetRadius               :: d -- Distance
                                           , planetHeatGen              :: d -- Energy
                                           , planetSpecificHeatCapacity :: d -- (Energy :/ Distance:^I3)
                                           }

data StarType = RedStar
              | BlueStar
              | YellowStar
              | Dwarf
              | Neutron
              | Pulsar

data Star d = Star { starId         :: UUID
                   , starType       :: StarType
                   , starDescriptor :: StarDescriptor d
                   }


data StarDescriptor d = StarDescriptor { starMass   :: d -- Mass
                                       , starRadius :: d -- Distance
                                       , starFuelConsumption :: d -- (Mass :/ Time)
                                       }



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Ellipse vec d = Ellipse { focalPoints :: (vec d, vec d) }

data Orbit vec d = Orbit { orbit            :: Ellipse vec d
                         , positionInOrbit  :: d -- Angle
                         }

type Interaction d = (Vector d {- Force -}, Vector d {- Impulse -})

--interaction :: (d, d) -> (d, d) -> Interaction d
--interaction x y = (MeasuredVal x Force, MeasuredVal y Impulse)


numF1 f (x1, x2)          = (f x1, f x2)
numF2 f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

instance (Num d) => Num (d, d) where
    (+) = numF2 (+)
    (-) = numF2 (-)
    (*) = numF2 (*)
    abs = numF1 abs
    signum = numF1 signum
    fromInteger i = numF1 fromInteger (i, i)

numF1' f1 f2 (x1, x2)          = (f1 x1, f2 x2)
numF2' f1 f2 (x1, x2) (y1, y2) = (f1 x1 y1, f2 x2 y2)

--instance (Num d) => Num (Interaction d) where
--    (+) = numF2' (+) (+)
--    (-) = numF2' (-) (-)
--    (*) = numF2' (*) (*)
--    abs = numF1' abs abs
--    signum = numF1' signum signum
--    fromInteger i = numF1' fromInteger fromInteger (i, i)

--    (x1, x2) + (y1, y2) = (x1 + y1, x2 + y2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Vector d = (d, d)

vecAbs (x, y) = sqrt (x**2 + y**2)
vecNorm (x, y) = (x / a, y / a)
              where a = vecAbs (x, y)
vecF  f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)
vecF' f (x1, x2) = (f x1, f x2)

type Position d = Vector d {- Distance -}

--class VectorOps vec where
--    vecAbs :: vec -> d
--
--instance (Floating d) => VectorOps (Vector d) where
--    vecAbs (x, y) = sqrt (x**2 + y**2)

data System d = System { systemId         :: UUID
                       , stellarBodies    :: [(StellarBodyContainer d, Position d)]
                       , artificialBodies :: [(ArtificialContainer d, Position d)]
                       }



class Body body where
    id      :: body -> UUID
    mass    :: body -> d -- Mass

class (Body body) => StellarBody body where

class (Body body) => ArtificialBody body where

instance Eq (StellarBodyContainer d) where
    StellarBodyContainer{getStellarBody=x} == StellarBodyContainer{getStellarBody=y} = U.id x == U.id y

data StellarBodyContainer d = forall body. (HasZero d, Eq (body d), StellarBody (body d))
    => StellarBodyContainer {
        getStellarBody :: body d
       }
data ArtificialContainer d  = forall body. (HasZero d, Eq (body d), ArtificialBody (body d))
    => ArtificialContainer {
        getArtificialBody :: body d
       }


type Effect a d = a -> a -> Interaction d


class HasPosition a where
    position :: a -> Vector d {- Distance -}
    distance :: a -> a -> Vector d -- Distance

--numF2'' :: () -> () -> Effect a d -> Effect a d -> Effect a d
--numF2'' f1 f2 e1 e2 a b =(f1 x1 y1, f2 x2 y2) :: Interaction d
--                   where (x1, x2) = e1 a b
--                         (y1, y2) = e2 a b

--instance Num (Effect a d) where
--    (+) = numF2'' (+) (+)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class SystemExec sys where
    execInteractions :: sys -> d -- Time -> sys

instance SystemExec (System d) where
    execInteractions = undefined


instance Body (StellarBodyContainer d) where
    id (StellarBodyContainer body) = U.id body
    mass (StellarBodyContainer body) = mass body

--instance HasZero (StellarBodyContainer d)

instance StellarBody (StellarBodyContainer d)

zeroFor :: StellarBodyContainer d -> d
zeroFor (StellarBodyContainer d) = zero


interact :: (Num d) => d -> [Effect a d] -> Effect a d
interact zero effects x y = foldr (+) (zeroVec, zeroVec) interractions
                            where zeroVec       = (zero, zero)
                                  interractions = map (($ x) . ($ y)) effects
--do effect <- effects
--                             effect x y
--                             mzero

effects :: (Body a, HasPosition a, Floating d, HasZero d) => (?g :: d) => [Effect a d]
effects = [gravityEffect]

applyInteraction a ap i = undefined

calculateInteractions :: (HasPosition (StellarBodyContainer d), Floating d, HasZero d) =>
                         (?g :: d) =>
                         System d -> d {- Time -} -> [(StellarBodyContainer d, Position d)]
calculateInteractions sys time = do (a, ap) <- stellarBodies sys --TODO
                                    (b, bp) <- stellarBodies sys
                                    let interaction' = U.interact (zeroFor a) effects a b
                                    if a /= b then return (a, applyInteraction a ap interaction')
                                              else mzero

calculateMovements :: (Body body, Fractional d) =>
                        d {- Time -}
                        -> [(body, (Vector d {- Force -}, Vector d {- Impulse -}))]
                        -> [(body, (Vector d {- Impulse -}, Position d))]

calculateMovements time input = do (obj, (force, imp)) <- input
                                   -- TODO collisions
                                   let impulse  = vecF' (time *) force
                                   let position = vecF' (\x -> x / mass obj * time) impulse
                                   return (obj, (impulse, position))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

gravityEffect ::  (Body a, HasPosition a, Floating d, HasZero d) => (?g :: d) => Effect a d
gravityEffect x y = (force, (zero, zero))
                 where dist     = distance x y
                       forceAbs = ?g * mass x * mass y / vecAbs dist ^ 2
                       norm     = vecNorm dist
                       force    = vecF' (forceAbs *) norm


impactEffect :: (HasZero d) => Effect a d
impactEffect x y = ((zero, zero), impulse)
                where impulse = undefined


