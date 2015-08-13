{-# LANGUAGE MultiParamTypeClasses #-}

module Universe.Objects.Celestial (

  CelestialBody(..)

, Star(..)
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


class (CelestialBody obj d coordSys id)           => Planet   obj d coordSys id
    where planetClass :: obj -> PlanetClass


class (CelestialBody obj d coordSys id)           => Comet    obj d coordSys id


class (CelestialBody obj d coordSys id)           => Asteroid obj d coordSys id



