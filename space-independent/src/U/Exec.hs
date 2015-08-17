{-# LANGUAGE MultiParamTypeClasses
           , ImplicitParams
         #-}

module U.Exec (

  SystemExec(..)
, SystemExecCache(..)
, calculateInteractions

--TODO move it
, GravitationalConstant(..)  -- Force :* Distance:^I2 :/ Mass:^I2

) where

import Control.Monad (mzero)

import Utils

import U.Defs
import U.Objects
--import U.Exec.Mutable

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class SystemExec sys d where
    execInteractions :: sys d -> d {- Time -} -> sys d

class SystemExecCache sys d where
    systemStellarStates :: sys d -> [StellarBodyEntry d]
    systemStellarState  :: sys d -> StellarBodyContainer d -> BodyState d

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data GravitationalConstant d = GravitationalConstant { getG :: d }  -- Force :* Distance:^I2 :/ Mass:^I2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance (SystemExecCache sys d, Num d) => HasPosition sys StellarBodyContainer d where
    position sys = snd . systemStellarState sys
    distance sys x y = c x - c y
                    where c = position sys

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

interact :: (Num d) => d -> [Effect a d] -> Effect a d
interact zero effects x y = foldr (+) (zeroVec, zeroVec) interractions
                            where zeroVec       = (zero, zero)
                                  interractions = map (($ x) . ($ y)) effects

zeroFor :: StellarBodyContainer d -> d
zeroFor (StellarBodyContainer d) = zero

-- !!!! --
effects :: (Body a d, HasPosition sys a d, Floating d, HasZero d) =>
           (?g :: GravitationalConstant d) =>
        sys d -> [Effect (a d) d]
effects sys = map ($ sys) [gravityEffect]
-- !!!! --

calculateInteractions :: (SystemExecCache System d, HasPosition System StellarBodyContainer d, Floating d, HasZero d) =>
                         (?g :: GravitationalConstant d) =>
                         System d -> d {- Time -} -> [StellarBodyEntry d]
calculateInteractions sys time = do (a, (_, ap)) <- stellarBodies sys --TODO
                                    (b, _) <- stellarBodies sys
                                    let (force, imp) = U.Exec.interact (zeroFor a) (effects sys) a b
                                    if a /= b then return $ calculateMovement time (a, (force, imp, ap))
                                              else mzero

--calculateMovements time = map (calculateMovement time)

calculateMovement :: (Body body d, Fractional d) =>
                        d {- Time -}
                        -> (body d, (Vector d {- Force -}, Vector d {- Impulse -}, Position d))
                        -> (body d, (Vector d {- Impulse -}, Position d))
calculateMovement time (obj, (force, imp, pos)) = (obj, (impulse, position)) -- TODO collisions
                                            where impulse  = imp + vecF (time *) force
                                                  position = pos + vecF (\x -> x / mass obj * time) impulse

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

gravityEffect ::  (Body a d, HasPosition sys a d, Floating d, HasZero d) =>
                  (?g :: GravitationalConstant d) =>
              sys d -> Effect (a d) d
gravityEffect sys x y = (force, (zero, zero))
                 where dist     = distance sys x y
                       forceAbs = getG ?g * mass x * mass y / vecAbs dist ** 2
                       norm     = vecNorm dist
                       force    = vecF (forceAbs *) norm


-- TODO
impactEffect :: (HasZero d) => Effect a d
impactEffect x y = ((zero, zero), impulse)
                where impulse = undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--instance SystemMExec (MSystem StellarBodyContainer ArtificialContainer) StellarBodyContainer ArtificialContainer d where
--    guardStellarInteraction = upd
                                            -- TODO other factors, like mass loss
--                                            return ()
--    execInteractions sys time =


