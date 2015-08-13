{-# LANGUAGE TypeOperators
           , ExistentialQuantification
--           , MultiParamTypeClasses
           , FlexibleInstances
--           , UndecidableInstances
--           , AllowAmbiguousTypes
--           , ScopedTypeVariables
           #-}

module U (

) where



import Data.UUID
import Control.Monad (mzero)

import Measures
import Utils


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



data Planet d = Planet{ planetId          :: UUID
                      , planetDescriptor  :: PlanetDescriptor d
                      }

data PlanetDescriptor d = PlanetDescriptor { planetMass                 :: MeasuredVal d Mass
                                           , planetRadius               :: MeasuredVal d Distance
                                           , planetHeatGen              :: MeasuredVal d Energy
                                           , planetSpecificHeatCapacity :: MeasuredVal d (Energy :/ Distance:^I3)
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


data StarDescriptor d = StarDescriptor { starMass   :: MeasuredVal d Mass
                                       , starRadius :: MeasuredVal d Distance
                                       , starFuelConsumption :: MeasuredVal d (Mass :/ Time)
                                       }



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Ellipse vec d = Ellipse { focalPoints :: (vec d, vec d) }

data Orbit vec d = Orbit { orbit            :: Ellipse vec d
                         , positionInOrbit  :: MeasuredVal d Angle
                         }

type Interaction d = (VectorMeasured d Force, VectorMeasured d Impulse)

interaction :: (d, d) -> (d, d) -> Interaction d
interaction x y = (MeasuredVal x Force, MeasuredVal y Impulse)


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

instance (Num d) => Num (Interaction d) where
    (+) = numF2' (+) (+)
    (-) = numF2' (-) (-)
    (*) = numF2' (*) (*)
    abs = numF1' abs abs
    signum = numF1' signum signum
    fromInteger i = numF1' fromInteger fromInteger (i, i)

--    (x1, x2) + (y1, y2) = (x1 + y1, x2 + y2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type VectorMeasured d m = MeasuredVal (d, d) m

type Position d = VectorMeasured d Distance


data System d = System { systemId         :: UUID
                       , stellarBodies    :: [(StellarBodyContainer d, Position d)]
                       , artificialBodies :: [(ArtificialContainer d, Position d)]
                       }



class Body body where
    id      :: body -> UUID
    mass    :: body -> MeasuredVal d Mass

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
    position :: a -> VectorMeasured d Distance
    distance :: a -> a -> MeasuredVal d Distance

--numF2'' :: () -> () -> Effect a d -> Effect a d -> Effect a d
--numF2'' f1 f2 e1 e2 a b =(f1 x1 y1, f2 x2 y2) :: Interaction d
--                   where (x1, x2) = e1 a b
--                         (y1, y2) = e2 a b

--instance Num (Effect a d) where
--    (+) = numF2'' (+) (+)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class SystemExec sys where
    execInteractions :: sys -> MeasuredVal d Time -> sys

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
interact zero effects x y = foldr (+) (zeroVec Force, zeroVec Impulse) interractions
                            where zeroVec       = MeasuredVal (zero, zero)
                                  interractions = map (($ x) . ($ y)) effects
--do effect <- effects
--                             effect x y
--                             mzero

effects = undefined

applyInteraction a ap i = undefined

calculateInteractions :: (Num d, HasZero d) => System d -> MeasuredVal d Time -> [(StellarBodyContainer d, Position d)]
calculateInteractions sys time = do (a, ap) <- stellarBodies sys --TODO
                                    (b, bp) <- stellarBodies sys
                                    let interaction' = U.interact (zeroFor a) effects a b
                                    if a /= b then return (a, applyInteraction a ap interaction')
                                              else mzero

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

_G :: (HasZero d) => MeasuredVal d Distance:^I3 :/ (Time:^I2 :* Mass)
_G = MeasuredVal zero Distance:^I3 :/ (Time:^I2 :* Mass)

gravityEffect :: (HasZero d) => Effect a d
gravityEffect x y = interaction force (zero, zero)
                 where -- forceAbs = measuredValue _G * measuredValue (mass x * mass y) / measuredValue (distance x y) ^ 2
                       force = undefined





