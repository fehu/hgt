{-# LANGUAGE MultiParamTypeClasses
           , ImplicitParams
         #-}

module U.Exec.Double (

) where

import U.Objects
import U.Exec

doubleG = GravitationalConstant 6.674e-11 -- Force :* Distance:^I2 :/ Mass:^I2

instance SystemExecCache System Double
--    systemStellarStates sys =

instance SystemExec System Double where
    execInteractions sys time = copySystem sys [] []
                              where interactionResult = let ?g = doubleG
                                                        in calculateInteractions sys time

