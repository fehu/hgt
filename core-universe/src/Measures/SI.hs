module Measures.SI (

) where

import Measures

instance Measure Time where
    measureSystem _ = SI
    measureName   _ = "second"

instance Measure Distance where
    measureSystem _ = SI
    measureName   _ = "meter"

instance Measure Mass where
    measureSystem _ = SI
    measureName   _ = "kilogram"

instance Measure Temperature where
    measureSystem _ = SI
    measureName   _ = "kelvin"

instance Measure Energy where
    measureSystem _ = SI
    measureName   _ = "watt"

--instance Measure Luminosity where
--    measureSystem _ = SI
--    measureName   _ = ""
