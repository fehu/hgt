{-# LANGUAGE MultiParamTypeClasses
            , FlexibleInstances
            #-}

module CelestialObjects (

) where

import Measures
import Universe.Objects
import Universe.Objects.Stellar
import Universe.Objects.Shape


data CoordinateSystem d = CoordinateSystem -- TODO

data AStar id d = AStar { starPosition           :: VectorMeasured d Distance
                        , starSpeed              :: VectorMeasured d (D' Distance)
                        , starPositionRelativeTo :: CoordinateSystem d
                        , starMass               :: MeasuredVal d Mass
                        , starRadius             :: MeasuredVal d Distance
                        , starNominalBrightness  :: MeasuredVal d Luminosity
                        , starId                 :: id
                        , spectrumClass'         :: StarSpectrumClass
                        , starLifeStage'         :: StarLifeStage
                        }

--instance Any Double (AStar String Double) CoordinateSystem where

resolveCoordinates :: coord -> originCS -> targetCS -> VectorMeasured d Distance
resolveCoordinates c cso cst = undefined --TODO

-- TODO is it possible to merge this one and resolveCoordinates ?
resolveSpeed :: speed -> originCS -> targetCS -> VectorMeasured d (D' Distance)
resolveSpeed s cso cst = undefined --TODO

resolverBrightness :: nominalBrightness -> relativeTo -> Maybe (MeasuredVal d Luminosity)
resolverBrightness nBrightness relTo = undefined --TODO


instance Any (AStar id d) d coordSys where
    restMass AStar{starMass=m} = m
    coordinates csys AStar{starPosition=c, starPositionRelativeTo=cs} = resolveCoordinates c cs csys
    speed       csys AStar{starSpeed=s,    starPositionRelativeTo=cs} = resolveSpeed       s cs csys

instance OpticallySeen (AStar id d) d coordSys where
    brightness cs AStar{starNominalBrightness=nb} = resolverBrightness nb cs
    lightSource   AStar{starNominalBrightness=nb} = LightGeneration nb

instance Physical (AStar id d) d coordSys where
    shape AStar{starRadius=r} = Spherical r

instance CelestialBody (AStar id d) d coordSys id where
    bodyId AStar{starId=id} = id
    rotationAxis  cs AStar{} = Nothing --TODO
    rotationSpeed cs AStar{} = Nothing --TODO

instance Star (AStar id d) d coordSys id where
    spectrumClass AStar{spectrumClass'=sc} = sc
    starLifeStage AStar{starLifeStage'=st} = st


--aStar = AStar 4 3 ""
