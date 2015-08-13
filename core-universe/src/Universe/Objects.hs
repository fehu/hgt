{-# LANGUAGE MultiParamTypeClasses #-}

module Universe.Objects (

  VectorMeasured

, LuminositySource(..)

, StellarSystem(..)
, CelestialBody(..)
, Artificial(..)

, Any(..)
, Physical(..)
, OpticallySeen(..)
, HasPropulsion(..)
, CompositionKnown(..)
, HasTrajectory(..)
, HasOrbit(..)

) where

import Data.Map
import Graphics.Rendering.OpenGL.GL.Tensor(Vector3)

import Measures
import Universe.Phys
import Universe.Objects.Shape
import Universe.Objects.Chemistry

type TODO = Int

data LuminositySource d = LightReflection { albedo :: d }
                        | LightGeneration { luminosity :: MeasuredVal d Luminosity }


                           -- --- --- -- --- --- --
   --- --- --- --- --- --- --- --- --- -- --- --- --- --- --- --- --- --- ---
-- --- --- --- --- --- --- --- --- Properties --- --- --- --- --- --- --- --- --
   --- --- --- --- --- --- --- --- --- -- --- --- --- --- --- --- --- --- ---
                            -- --- --- -- --- --- --

class Any obj d coordSys where
    restMass    :: obj -> MeasuredVal d Mass
    coordinates :: coordSys -> obj -> VectorMeasured d Distance
    speed       :: coordSys -> obj -> VectorMeasured d (D' Distance) --MeasuredVal (Vector3 d) (D' Distance)


class (Any obj d coordSys) =>
    OpticallySeen obj d coordSys where
        brightness    :: coordSys -> obj -> Maybe (MeasuredVal d Luminosity)
        lightSource   ::             obj -> LuminositySource d


class (Any obj d coordSys) =>
    Physical obj d coordSys         where shape         :: obj -> Shape d


class (Any obj d coordSys) =>
    CompositionKnown obj d coordSys where composedOf    :: obj -> Composition d


class (Any obj d coordSys) =>
    HasTrajectory obj d coordSys    where trajectoryOf  :: obj -> timeLapse -> Position d relativeTo


class (Any obj d coordSys) =>
    HasOrbit obj d coordSys         where orbitOf       :: obj -> Orbit d


class (Any obj d coordSys) =>
    HasPropulsion obj d coordSys    where force         :: obj -> VectorMeasured d Force


class (Any obj d coordSys) =>
    WarpCapable obj d coordSys      where               -- TODO



                            -- --- --- -- --- --- --
   --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
-- --- --- --- --- --- --- --- --- Objects --- --- --- --- --- --- --- --- --- --
   --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                            -- --- --- -- --- --- --

class (Any body d coordSys) => StellarSystem obj d coordSys id body where
    systemId :: obj -> id
    bodies   :: obj -> [body]


class (Physical obj d coordSys, OpticallySeen obj d coordSys) =>
    CelestialBody obj d coordSys id where
        bodyId :: obj -> id
        rotationAxis  :: coordSys -> obj -> Maybe (Vector3 d)
        rotationSpeed :: coordSys -> obj -> Maybe (MeasuredVal d (D' Angle))


class (Physical obj d coordSys, OpticallySeen obj d coordSys) =>
    Artificial obj d coordSys id where
        objectId :: obj -> id
        owner    :: obj -> TODO


--class (Artificial obj coordSys id d, HasPropulsion d obj coordSys) =>
--    Transport obj coordSys id d where
--        massCapacity    :: obj -> MeasuredVal d Mass
--        volumeCapacity  :: obj -> MeasuredVal d (Pow3 Distance)
--
