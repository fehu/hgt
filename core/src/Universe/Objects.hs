{-# LANGUAGE MultiParamTypeClasses #-}

module Universe.Objects (
  AnyObject(..)
) where

import Data.Map
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)

import Measures


class AnyObject d obj coordSys where
    restMass    :: obj -> MeasuredVal d Mass
    coordinates :: coordSys -> obj -> MeasuredVal (Vector3 d) Distance
    speed       :: coordSys -> obj -> MeasuredVal (Vector3 d) (D' Distance)


