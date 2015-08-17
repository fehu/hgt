{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
         #-}

module U.Objects (

  Planet(..)
, PlanetDescriptor(..)

, Star(..)
, StarType(..)
, StarDescriptor(..)

, System(..)
, copySystem

, StellarBodyEntry
, BodyState

) where


import Data.UUID

import U.Defs

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

instance Body Planet d where
    id = planetId
    mass = planetMass . planetDescriptor

instance StellarBody Planet d


--mapBinOperator op f x y = op (f x) (f y)
--
--instance Eq (Planet d) where
--    (==) = mapBinOperator (==) planetId

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

instance Body Star d where
    id   = starId
    mass = starMass . starDescriptor

instance StellarBody Star d

--instance Eq (Star d) where
--    (==) = mapBinOperator (==) starId


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type BodyState d = (Vector d, Position d)

type StellarBodyEntry d = (StellarBodyContainer d, BodyState d)

data System d = System { systemId         :: UUID
                       , stellarBodies    :: [StellarBodyEntry d]
                       , artificialBodies :: [(ArtificialContainer d, Position d)]
                       }
              deriving Show

copySystem (System id _ _) = System id


