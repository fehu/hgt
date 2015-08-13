module Universe.Objects.Shape (
    Shape(..)
) where

import Measures

data Shape d = Spherical (MeasuredVal d Distance)
             | ShapeTODO -- TODO

