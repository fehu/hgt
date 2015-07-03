module Utils (
  Point(..)
, Rect(..)
) where

data Point a = Point {
                x :: a
              , y :: a
              } deriving (Show, Eq, Ord)

data Rect a = Rect { topLeft       :: Point a
                   , bottomRight   :: Point  a
                   } deriving (Show, Eq)

