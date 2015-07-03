{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Universe (

) where

import Data.Map
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)

data Universe obj = Universe{ uObjects  :: [obj] }


 --- -- Measures -- ---

data MeasureSystem = SI
                   | LargeScale
                   | Custom

class Measure m where
    measureName   :: m -> String
    measureSystem :: m -> MeasureSystem

data Time        = Time
data Distance    = Distance
data Mass        = Mass
data Luminosity  = Luminosity
data Temperature = Temperature
data Energy      = Energy

data Mult a b = Mult a b
data Div  a b = Div  a b
data Pow2 a   = Pow  a

type D'  a = Div a Time
type D'' a = Div a (Pow2 Time)

class (Measure m) => Measured d m where
    measuredValue :: d
    measure       :: m

class (Measured d from, Measured d to) => MeasureConversion d from to where
    convert :: from -> to

data (Measure m) => MeasuredVal d m = MeasuredVal d m


 --- -- Physical Laws -- ---

class (Measure m) => GravityField obj m where
    gravityPotential :: obj a -> m



 --- -- Objects -- ---

class AnyObject d obj coordSys where
    restMass    :: obj -> MeasuredVal d Mass
    coordinates :: coordSys -> obj -> MeasuredVal (Vector3 d) Distance
    speed       :: coordSys -> obj -> MeasuredVal (Vector3 d) (D' Distance)














data ObjectPoint id d = ObjectPoint (MeasuredVal d Mass)
                                    id -- relativeTo
                                    (MeasuredVal (Vector3 d) Distance)
                                    (MeasuredVal (Vector3 d) (D' Distance))


instance AnyObject d (ObjectPoint id d) Int where
    restMass         (ObjectPoint mass _ _ _) = mass
    coordinates cSys (ObjectPoint _ id pos _) = pos -- todo !! relativeTo
    speed cSys       (ObjectPoint _ id _ v)   = v   -- todo !! relativeTo


