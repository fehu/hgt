{-# LANGUAGE MultiParamTypeClasses
            , FlexibleInstances
            #-}

module CelestialObjects (

) where

import Measures
import Universe.Objects
import Universe.Objects.Celestial
import Universe.Objects.Shape

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

resolveCoordinates :: coord -> originCS -> targetCS -> VectorMeasured d Distance
resolveCoordinates c cso cst = undefined --TODO

-- TODO is it possible to merge this one and resolveCoordinates ?
resolveSpeed :: speed -> originCS -> targetCS -> VectorMeasured d (D' Distance)
resolveSpeed s cso cst = undefined --TODO

resolveBrightness :: nominalBrightness -> relativeTo -> Maybe (MeasuredVal d Luminosity)
resolveBrightness nBrightness relTo = undefined --TODO


instance Any (AStar id d) d coordSys where
    restMass AStar{starMass=m} = m
    coordinates csys AStar{starPosition=c, starPositionRelativeTo=cs} = resolveCoordinates c cs csys
    speed       csys AStar{starSpeed=s,    starPositionRelativeTo=cs} = resolveSpeed       s cs csys

instance OpticallySeen (AStar id d) d coordSys where
    brightness cs AStar{starNominalBrightness=nb} = resolveBrightness nb cs
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data APlanet id d = APlanet { planetId                  :: id
                            , planetPosition            :: VectorMeasured d Distance
                            , planetPositionRelativeTo  :: CoordinateSystem d
                            , planetSpeed               :: VectorMeasured d (D' Distance)
                            , planetMass                :: MeasuredVal d Mass
                            , planetAlbedo              :: d
                            , planetRadius              :: MeasuredVal d Distance
                            , planetClass_              :: PlanetClass
                            }

resolveBrightnessByAlbedo :: coordSys -> obj -> Maybe (MeasuredVal d Luminosity)
resolveBrightnessByAlbedo _ _ = undefined --TODO

instance Any (APlanet id d) d coordSys where
    restMass         APlanet{planetMass=m} = m
    coordinates csys APlanet{planetPositionRelativeTo=posRel, planetPosition=pos} = resolveCoordinates pos posRel csys
    speed       csys APlanet{planetPositionRelativeTo=posRel, planetSpeed=speed}  = resolveSpeed speed posRel csys

instance OpticallySeen (APlanet id d) d coordSys where
    lightSource APlanet{planetAlbedo=a} = LightReflection a
    brightness = resolveBrightnessByAlbedo

instance Physical (APlanet id d) d coordSys where
    shape APlanet{planetRadius=r} = Spherical r

instance CelestialBody (APlanet id d) d coordSys id where
    bodyId APlanet{planetId=id} = id
    rotationAxis  cs APlanet{} = Nothing --TODO
    rotationSpeed cs APlanet{} = Nothing --TODO

instance Planet (APlanet id d) d coordSys id where
    planetClass = planetClass_

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data ASystem id d body = ASystem { systemId_ :: id
                                 , bodies_   :: [body]
                                 }


instance (Any body d coordSys) => StellarSystem (ASystem id d body) d coordSys id body where
    systemId = systemId_
    bodies   = bodies_






-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (StellarSystem sys d coordSys id body) => SystemExec sys time d id coordSys body where
    exec :: time -> sys -> sys


--instance  (Any body d coordSys) => SystemExec (ASystem id d body) time d id coordSys body where
--    exec time sys = let x = do a <- bodies sys
--                               do b <- bodies sys
--                                  if a /= b then undefined--let interaction
--                                            else []
--                    in ASystem (systemId sys) []










