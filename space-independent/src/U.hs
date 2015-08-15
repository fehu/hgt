--{-# LANGUAGE TypeOperators
--           , ExistentialQuantification
--           , FlexibleInstances
--           , ImplicitParams
--           , MultiParamTypeClasses
--           #-}

module U (

) where



--import Data.UUID
import Data.UUID.V1(nextUUID)

import GHC.Ptr

import U.Objects
import U.Defs
import U.Exec

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

mapBinOperator op f x y = op (f x) (f y)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


data ObjectState d = ObjectState (Ptr (Vector d)) {- Impulse -}
                                 (Ptr (Position d))


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--systemStellarStates sys





main :: IO()
main = do Just starId <- nextUUID
          let starD = StarDescriptor 2e30 7e5 1e5 :: StarDescriptor Double
          let star  = Star starId RedStar starD
          let starC = (StellarBodyContainer star, ((0, 0), (0, 0)))

          Just planetId <- nextUUID
          let planetD = PlanetDescriptor 6e24 6400 1e5 6e11
          let planet  = Planet planetId planetD
          let planetS = ((0, 0), (0, 0))
          let planetC = (StellarBodyContainer planet, planetS)

          Just systemId <- nextUUID
          let system = System systemId [starC, planetC] []

          putStrLn $ "init: " ++ show system

--          let sys2 =



