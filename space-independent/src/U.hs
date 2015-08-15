{-# LANGUAGE TypeOperators
           , ExistentialQuantification
           , FlexibleInstances
           , ImplicitParams
           , MultiParamTypeClasses
           #-}

module U (

) where



import Data.UUID
import Data.UUID.V1(nextUUID)

import Control.Monad (mzero)
import GHC.Ptr

import Utils


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



data Planet d = Planet{ planetId          :: UUID
                      , planetDescriptor  :: PlanetDescriptor d
                      }
              deriving Show

data PlanetDescriptor d = PlanetDescriptor { planetMass                 :: d -- Mass
                                           , planetRadius               :: d -- Distance
                                           , planetHeatGen              :: d -- Energy
                                           , planetSpecificHeatCapacity :: d -- (Energy :/ Distance:^I3)
                                           }
                        deriving Show

data StarType = RedStar
              | BlueStar
              | YellowStar
              | Dwarf
              | Neutron
              | Pulsar
              deriving Show

data Star d = Star { starId         :: UUID
                   , starType       :: StarType
                   , starDescriptor :: StarDescriptor d
                   }
            deriving Show


data StarDescriptor d = StarDescriptor { starMass   :: d -- Mass
                                       , starRadius :: d -- Distance
                                       , starFuelConsumption :: d -- (Mass :/ Time)
                                       }
                      deriving Show

mapBinOperator op f x y = op (f x) (f y)


instance HasZero Double where
    zero = 0

instance Body Star d where
    id   = starId
    mass = starMass . starDescriptor

instance StellarBody Star d

instance Eq (Star d) where
    (==) = mapBinOperator (==) starId

instance Body Planet d where
    id = planetId
    mass = planetMass . planetDescriptor

instance StellarBody Planet d

instance Eq (Planet d) where
    (==) = mapBinOperator (==) planetId


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Ellipse vec d = Ellipse { focalPoints :: (vec d, vec d) } deriving Show

data Orbit vec d = Orbit { orbit            :: Ellipse vec d
                         , positionInOrbit  :: d -- Angle
                         }
                 deriving Show

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Vector d = (d, d)

vecAbs (x, y) = sqrt (x**2 + y**2)
vecNorm (x, y) = (x / a, y / a)
              where a = vecAbs (x, y)
--vecF  f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)
vecF f (x1, x2) = (f x1, f x2)

type Position d = Vector d {- Distance -}

data System d = System { systemId         :: UUID
                       , stellarBodies    :: [StellarBodyState d]
                       , artificialBodies :: [(ArtificialContainer d, Position d)]
                       }
              deriving Show

copySystem (System id _ _) = System id

class Body body d where
    id      :: body d -> UUID
    mass    :: body d -> d -- Mass

class (Body body d) => StellarBody body d where

class (Body body d) => ArtificialBody body d where

instance Eq (StellarBodyContainer d) where
    StellarBodyContainer{getStellarBody=x} == StellarBodyContainer{getStellarBody=y} = U.id x == U.id y

data StellarBodyContainer d = forall body. (HasZero d, Eq (body d), Show (body d), StellarBody body d)
    => StellarBodyContainer {
        getStellarBody :: body d
       }

data ArtificialContainer d  = forall body. (HasZero d, Eq (body d), Show (body d), ArtificialBody body d)
    => ArtificialContainer {
        getArtificialBody :: body d
       }

instance Show (StellarBodyContainer d) where
    show (StellarBodyContainer b) = show b

instance Show (ArtificialContainer d) where
    show (ArtificialContainer b) = show b


type Effect a d = a -> a -> Interaction d


class HasPosition sys a d where
    position :: sys d -> a d -> Vector d {- Distance -}
    distance :: sys d -> a d -> a d -> Vector d -- Distance

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class SystemExec sys d where
    execInteractions :: sys d -> d {- Time -} -> sys d

class SystemExecCache sys d where
    systemStellarStates     :: sys d -> [StellarBodyState d]
--    systemArtificialStates  :: sys d -> [ArtificialBodyState d]

instance Body StellarBodyContainer d where
    id (StellarBodyContainer body) = U.id body
    mass (StellarBodyContainer body) = mass body


zeroFor :: StellarBodyContainer d -> d
zeroFor (StellarBodyContainer d) = zero


data ObjectState d = ObjectState (Ptr (Vector d)) {- Impulse -}
                                 (Ptr (Position d))

type StellarBodyState d = (StellarBodyContainer d, (Vector d, Position d))

interact :: (Num d) => d -> [Effect a d] -> Effect a d
interact zero effects x y = foldr (+) (zeroVec, zeroVec) interractions
                            where zeroVec       = (zero, zero)
                                  interractions = map (($ x) . ($ y)) effects

effects :: (Body a d, HasPosition sys a d, Floating d, HasZero d) => (?g :: d) => sys d -> [Effect (a d) d]
effects sys = map ($ sys) [gravityEffect]

calculateInteractions :: (SystemExecCache System d, HasPosition System StellarBodyContainer d, Floating d, HasZero d) =>
                         (?g :: d) =>
                         System d -> d {- Time -} -> [StellarBodyState d]
calculateInteractions sys time = do (a, (_, ap)) <- stellarBodies sys --TODO
                                    (b, _) <- stellarBodies sys
                                    let (force, imp) = U.interact (zeroFor a) (effects sys) a b
                                    if a /= b then return $ calculateMovement time (a, (force, imp, ap))
                                              else mzero

--calculateMovements time = map (calculateMovement time)

calculateMovement :: (Body body d, Fractional d) =>
                        d {- Time -}
                        -> (body d, (Vector d {- Force -}, Vector d {- Impulse -}, Position d))
                        -> (body d, (Vector d {- Impulse -}, Position d))
calculateMovement time (obj, (force, imp, pos)) = (obj, (impulse, position)) -- TODO collisions
                                            where impulse  = imp + vecF (time *) force
                                                  position = pos + vecF (\x -> x / mass obj * time) impulse

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

gravityEffect ::  (Body a d, HasPosition sys a d, Floating d, HasZero d) => (?g :: d) => sys d -> Effect (a d) d
gravityEffect sys x y = (force, (zero, zero))
                 where dist     = distance sys x y
                       forceAbs = ?g * mass x * mass y / vecAbs dist ** 2
                       norm     = vecNorm dist
                       force    = vecF (forceAbs *) norm


-- TODO
impactEffect :: (HasZero d) => Effect a d
impactEffect x y = ((zero, zero), impulse)
                where impulse = undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance (SystemExecCache sys d) => HasPosition sys StellarBodyContainer d where

--systemStellarStates sys

doubleG = 6.674e-11 :: Double -- Force :* Distance:^I2 :/ Mass:^I2

instance SystemExecCache System Double

instance SystemExec System Double where
    execInteractions sys time = copySystem sys [] []
                              where interactionResult = let ?g = doubleG
                                                        in calculateInteractions sys time



main :: IO()
main = do Just starId <- nextUUID
          let starD = StarDescriptor 2e30 7e5 1e5 :: StarDescriptor Double
          let star  = Star starId RedStar starD
          let starC = (StellarBodyContainer star, ((0, 0), (0, 0)))

          Just planetId <- nextUUID
          let planetD = PlanetDescriptor 6e24 6400 1e5 6e11
          let planet  = Planet planetId planetD
          let planetS = ((0, 0), (0, 0))
          let planetC = (StellarBodyContainer planet, planetS)

          Just systemId <- nextUUID
          let system = System systemId [starC, planetC] []

          putStrLn $ "init: " ++ show system

--          let sys2 =



