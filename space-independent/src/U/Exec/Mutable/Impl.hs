{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ImplicitParams
         #-}
module U.Exec.Mutable.Impl (

) where

import Control.Monad (mzero)

import Utils

import U.Objects as O
import U.Defs    as D
import U.Exec (GravitationalConstant )
import U.Exec.Mutable as E

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

zeroForS :: StellarBodyContainer d -> d
zeroForS (StellarBodyContainer d) = zero


interactObj :: (Num d) => d -> [Effect a d] -> Effect a d
interactObj zero effects x y = foldr (+) (zeroVec, zeroVec) interractions
                            where zeroVec       = (zero, zero)
                                  interractions = map (($ x) . ($ y)) effects


calculateMovement :: (Body body d, Fractional d) =>
                        d {- Time -}
                        -> (body d, (Vector d {- Force -}, Vector d {- Impulse -}, Position d))
                        -> (body d, BodyState d)
calculateMovement time (obj, (force, imp, pos)) = (obj, (impulse, position)) -- TODO collisions
                                            where impulse  = imp + vecF (time *) force
                                                  position = pos + vecF (\x -> x / mass obj * time) impulse

calculateStellarInteractions :: ( MutableSystem sys StellarBodyContainer ArtificialContainer d
                         , HasPosition sys StellarBodyContainer d
                         , Floating d
                         , HasZero d) =>
                            ( ?g :: GravitationalConstant d
                            , ?effects :: [Effect (StellarBodyContainer d) d])
                                => (StellarBodyEntry d -> IO (StellarBodyEntry d))
                                    -> sys d
                                    -> d {- Time -}
                                    -> IO [StellarBodyEntry d]

calculateStellarInteractions = calculateInteractions (\a b -> interactObj (zeroForS a) ?effects a b)

calculateInteractions f g sys time = do get <- E.fullList sys
                                        let l = do (a, (_, ap)) <- get
                                                   (b, _)       <- get
                                                   let (force, imp) = f a b
                                                   if a /= b then return (g $ calculateMovement time (a, (force, imp, ap)))
                                                             else mzero
                                        sequence l

applyStellarInteractions sys = calculateStellarInteractions f sys
                        where f p = uncurry (upd sys) p >> return p







