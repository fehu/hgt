{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Universe (

  Universe(..)

, Measure, Measured, MeasureConversion

, GravityField

) where

import Measures
import Universe.PhysicalLaws
import Universe.Objects

import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)

--data Universe obj = Universe{ uObjects  :: [obj] }

class Universe u where
    uObjects :: u -> [obj]




 --- -- TODO -- ---
data ObjectPoint id d = ObjectPoint (MeasuredVal d Mass)
                                    id -- relativeTo
                                    (MeasuredVal (Vector3 d) Distance)
                                    (MeasuredVal (Vector3 d) (D' Distance))


instance Any d (ObjectPoint id d) Int where
    restMass         (ObjectPoint mass _ _ _) = mass
    coordinates cSys (ObjectPoint _ id pos _) = pos -- todo !! relativeTo
    speed cSys       (ObjectPoint _ id _ v)   = v   -- todo !! relativeTo


