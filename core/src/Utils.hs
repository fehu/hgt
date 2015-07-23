module Utils (
  Point(..)
, Rect(..)

, HasZero(zero)

, scalaGroupBy

) where

import Data.Function (on)
import Data.List     (groupBy, sortBy)

data Point a = Point {
                x :: a
              , y :: a
              } deriving (Show, Eq, Ord)

data Rect a = Rect { topLeft       :: Point a
                   , bottomRight   :: Point  a
                   } deriving (Show, Eq)


-- `algebra` package breaks some dpendencies, so ...
class (Num a) => HasZero a where
    zero :: a

scalaGroupBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
scalaGroupBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)
