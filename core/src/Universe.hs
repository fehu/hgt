{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Universe (

  Universe

, Measure, Measured, MeasureConversion

, GravityField

, AnyObject

) where

import Measures
import Universe.PhysicalLaws
import Universe.Objects

import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)

data Universe obj = Universe{ uObjects  :: [obj] }






 --- -- TODO -- ---
data ObjectPoint id d = ObjectPoint (MeasuredVal d Mass)
                                    id -- relativeTo
                                    (MeasuredVal (Vector3 d) Distance)
                                    (MeasuredVal (Vector3 d) (D' Distance))


instance AnyObject d (ObjectPoint id d) Int where
    restMass         (ObjectPoint mass _ _ _) = mass
    coordinates cSys (ObjectPoint _ id pos _) = pos -- todo !! relativeTo
    speed cSys       (ObjectPoint _ id _ v)   = v   -- todo !! relativeTo


