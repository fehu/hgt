{-# LANGUAGE MultiParamTypeClasses #-}

module Universe.Objects.Stellar (

  Star(..)
, StarSpectrumClass(..)
, StarLifeStage(..)

, Planet(..)
, PlanetClass(..)

, Comet(..)
, Asteroid(..)

) where


import Universe.Objects


data StarSpectrumClass = TODO  -- TODO
data StarLifeStage     = TODO' -- TODO



data PlanetClass = Todo -- TODO



class (CelestialBody obj coordSys id d) =>
    Star obj coordSys id d where spectrumClass :: obj -> StarSpectrumClass
                                 starLifeStage :: obj -> StarLifeStage

class (CelestialBody obj coordSys id d)     => NonStar  obj coordSys id d



class (NonStar obj coordSys id d)           => Planet   obj coordSys id d
    where planetClass :: obj -> PlanetClass


class (NonStar obj coordSys id d)           => Comet    obj coordSys id d


class (NonStar obj coordSys id d)           => Asteroid obj coordSys id d



