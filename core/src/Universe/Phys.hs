{-# LANGUAGE MultiParamTypeClasses #-}
module Universe.Phys (

  VectorMeasure
, Angle3D

, Position(..)
, Pose(..)

, Orbit(..)

) where

import Graphics.Rendering.OpenGL.GL.Tensor (Vector2, Vector3)

import Measures



type VectorMeasure d m = Measured (Vector3 d) m

type Angle3D d = Measured (Vector2 d) Angle




data Position d relativeTo = Position { coordsVector     :: VectorMeasure d Distance
                                      , coordsRelativeTo :: relativeTo
                                      }

data Pose d relativeTo = Pose { posePosition :: Position d relativeTo
                              , pose         :: Angle3D d
                              }

data Orbit d = TODO -- TODO
