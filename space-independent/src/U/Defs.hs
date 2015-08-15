{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , ExistentialQuantification
--           , UndecidableInstances
         #-}
module U.Defs (

  Vector
, vecF, vecAbs, vecNorm
, Position
, Interaction
, Effect

, Ellipse(..)
, Orbit(..)

, Body(..)
, StellarBody(..)
, ArtificialBody(..)

, StellarBodyContainer(..)
, ArtificialContainer(..)

, HasPosition(..)

) where

import Utils

import Data.UUID

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Vector d = (d, d)
type Position d = Vector d {- Distance -}

vecAbs (x, y) = sqrt (x**2 + y**2)
vecNorm (x, y) = (x / a, y / a)
              where a = vecAbs (x, y)
vecF f (x1, x2) = (f x1, f x2)


type Interaction d = (Vector d {- Force -}, Vector d {- Impulse -})

type Effect a d = a -> a -> Interaction d

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

instance HasZero Double where
    zero = 0



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Ellipse vec d = Ellipse { focalPoints :: (vec d, vec d) } deriving Show

data Orbit vec d = Orbit { orbit            :: Ellipse vec d
                         , positionInOrbit  :: d -- Angle
                         }
                 deriving Show

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Body body d where
    id      :: body d -> UUID
    mass    :: body d -> d -- Mass

class (Body body d) => StellarBody body d where

class (Body body d) => ArtificialBody body d where


--mapBinOperator op f x y = op (f x) (f y)
--
--instance (Body body d) => Eq (body d) where
--    (==) = mapBinOperator (==) U.Defs.id

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data StellarBodyContainer d = forall body. (HasZero d, Show (body d), StellarBody body d)
    => StellarBodyContainer {
        getStellarBody :: body d
       }

data ArtificialContainer d  = forall body. (HasZero d, Show (body d), ArtificialBody body d)
    => ArtificialContainer {
        getArtificialBody :: body d
       }



instance Eq (StellarBodyContainer d) where
    StellarBodyContainer{getStellarBody=x} == StellarBodyContainer{getStellarBody=y} = U.Defs.id x == U.Defs.id y

instance Eq (ArtificialContainer d) where
    ArtificialContainer{getArtificialBody=x} == ArtificialContainer{getArtificialBody=y} = U.Defs.id x == U.Defs.id y



instance Show (StellarBodyContainer d) where
    show (StellarBodyContainer b) = show b

instance Show (ArtificialContainer d) where
    show (ArtificialContainer b) = show b



instance Body StellarBodyContainer d where
    id (StellarBodyContainer body)   = U.Defs.id body
    mass (StellarBodyContainer body) = mass body

instance Body ArtificialContainer d where
    id (ArtificialContainer body)   = U.Defs.id body
    mass (ArtificialContainer body) = mass body


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class HasPosition sys a d where
    position :: sys d -> a d -> Vector d {- Distance -}
    distance :: sys d -> a d -> a d -> Vector d -- Distance

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



