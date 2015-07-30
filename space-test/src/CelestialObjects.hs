{-# LANGUAGE MultiParamTypeClasses
            , FlexibleInstances
            #-}

module CelestialObjects (

) where

import Measures
import Universe.Objects
import Universe.Objects.Stellar


data CoordinateSystem d = CoordinateSystem -- TODO

data AStar id d = AStar { starPosition      :: VectorMeasured d Distance
                        , starSpeed         :: VectorMeasured d (D' Distance)
                        , starPositionRelTo :: CoordinateSystem d
                        , starMass          :: MeasuredVal d Mass
                        , starBrightness    :: MeasuredVal d Luminosity
                        , starId            :: id
                        , spectrumClass'    :: StarSpectrumClass
                        , starLifeStage'    :: StarLifeStage
                        }

--instance Any Double (AStar String Double) CoordinateSystem where

resolveCoordinates :: coord -> originCS -> targetCS -> VectorMeasured d Distance
resolveCoordinates c cso cst = undefined --TODO

-- TODO is it possible to merge this one and resolveCoordinates ?
resolveSpeed :: speed -> originCS -> targetCS -> VectorMeasured d (D' Distance)
resolveSpeed s cso cst = undefined --TODO


instance Any (AStar id d) d coordSys where
    restMass AStar{starMass=m} = m
    coordinates csys AStar{starPosition=c, starPositionRelTo=cs} = resolveCoordinates c cs csys
    speed csys AStar{starSpeed=s, starPositionRelTo=cs} = resolveSpeed s cs csys

instance OpticallySeen (AStar id d) d coordSys

instance Physical (AStar id d) d coordSys

instance CelestialBody (AStar id d) d coordSys id

instance Star (AStar id d) d coordSys id


--aStar = AStar 4 3 ""
