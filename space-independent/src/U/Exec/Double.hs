{-# LANGUAGE MultiParamTypeClasses
           , ImplicitParams
         #-}

module U.Exec.Double (

) where

import U.Objects
import U.Exec

doubleG = 6.674e-11 :: Double -- Force :* Distance:^I2 :/ Mass:^I2

instance SystemExecCache System Double

instance SystemExec System Double where
    execInteractions sys time = copySystem sys [] []
                              where interactionResult = let ?g = doubleG
                                                        in calculateInteractions sys time

