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

class Any d obj coordSys where
    restMass    :: obj -> MeasuredVal d Mass
    coordinates :: coordSys -> obj -> VectorMeasured d Distance
    speed       :: coordSys -> obj -> VectorMeasured d (D' Distance) --MeasuredVal (Vector3 d) (D' Distance)


class (Any d obj coordSys) =>
    OpticallySeen d obj coordSys where
        brightness    :: coordSys -> obj -> Maybe (MeasuredVal d Luminosity)
        lightSource   ::             obj -> LuminositySource d


class (Any d obj coordSys) =>
    Physical d obj coordSys         where shape         :: obj -> Shape d


class (Any d obj coordSys) =>
    CompositionKnown d obj coordSys where composedOf    :: obj -> Composition d


class (Any d obj coordSys) =>
    HasTrajectory d obj coordSys    where trajectoryOf  :: obj -> timeLapse -> Position d relativeTo


class (Any d obj coordSys) =>
    HasOrbit d obj coordSys         where orbitOf       :: obj -> Orbit d


class (Any d obj coordSys) =>
    HasPropulsion d obj coordSys    where force         :: obj -> VectorMeasured d Force


class (Any d obj coordSys) =>
    WarpCapable d obj coordSys      where               -- TODO



                            -- --- --- -- --- --- --
   --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
-- --- --- --- --- --- --- --- --- Objects --- --- --- --- --- --- --- --- --- --
   --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
                            -- --- --- -- --- --- --

class StellarSystem id d obj body where
    systemId :: obj -> id
    bodies   :: obj -> [body]


class (Physical d obj coordSys, OpticallySeen d obj coordSys) =>
    CelestialBody obj coordSys id d where
        bodyId :: obj -> id
        rotationAxis  :: coordSys -> obj -> Maybe (Vector3 d)
        rotationSpeed :: coordSys -> obj -> Maybe (MeasuredVal d (D' Angle))


class (Physical d obj coordSys, OpticallySeen d obj coordSys) =>
    Artificial obj coordSys id d where
        objectId :: obj -> id
        owner    :: obj -> TODO


--class (Artificial obj coordSys id d, HasPropulsion d obj coordSys) =>
--    Transport obj coordSys id d where
--        massCapacity    :: obj -> MeasuredVal d Mass
--        volumeCapacity  :: obj -> MeasuredVal d (Pow3 Distance)
--
