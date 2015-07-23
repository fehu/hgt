{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleContexts
           , FlexibleInstances
           , Rank2Types
--           , ConstraintKinds
           #-}

module Universe.Phys (

  VectorMeasured(..)
, Angle3D(..)

, Position(..)
, Pose(..)

, Orbit(..)

) where

import Graphics.Rendering.OpenGL.GL.Tensor (Vector2, Vector3)

import Measures



--data MeasuredVal d m = forall a . Measured a d m => MeasuredVal a
--data VectorMeasured d m = forall a . Measured a (Vector3 d) m => VectorMeasured a
--data Angle3D d = forall a . Measured a (Vector2 d) Angle => Angle3D a

type VectorMeasured d m = MeasuredVal (Vector3 d) m

type Angle3D d = MeasuredVal (Vector2 d) Angle


--instance (Measure m) => Measured (MeasuredVal d m) d m where
--    measured measure value =MeasuredVal value measure
--    measuredValue


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Position d relativeTo = Position { coordsVector     :: VectorMeasured d Distance
                                      , coordsRelativeTo :: relativeTo
                                      }

data Pose d relativeTo = Pose { posePosition :: Position d relativeTo
                              , pose         :: Angle3D d
                              }

data Orbit d = TODO -- TODO
