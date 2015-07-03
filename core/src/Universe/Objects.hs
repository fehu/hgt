{-# LANGUAGE MultiParamTypeClasses #-}

module Universe.Objects (

  VectorMeasure

, Any(..)
, Physical(..)
, OpticallySeen(..)
, HasPropulsion(..)
, CompositionKnown(..)
, HasTrajectory(..)
, HasOrbit(..)

) where

import Data.Map
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)

import Measures
import Universe.Phys
import Universe.Objects.Shape
import Universe.Objects.Chemistry


type VectorMeasure d m = MeasuredVal (Vector3 d) m

type TODO = Int

data StellarSystem id d = StellarSystem { systemId :: id
                                        , stars    :: [TODO]
                                        }


--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
--- --- --- --- --- --- --- --- Typeclasses --- --- --- --- --- --- --- ---
--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

class Any d obj coordSys where
    restMass    :: obj -> MeasuredVal d Mass
    coordinates :: coordSys -> obj -> VectorMeasure d Distance
    speed       :: coordSys -> obj -> MeasuredVal (Vector3 d) (D' Distance)


class Physical d obj where
    shape :: obj -> Shape d


class OpticallySeen d obj relativeTo where
    brightness :: relativeTo -> obj -> Maybe (MeasuredVal d Luminosity)


class HasPropulsion d obj where
    force :: obj -> VectorMeasure d Force


class CompositionKnown d obj where
    composedOf :: obj -> Composition d


class HasTrajectory d obj where
    trajectoryOf :: obj -> Trajectory d


class HasOrbit d obj where
    orbitOf :: obj -> Orbit d


class WarpCapable d obj where -- TODO




