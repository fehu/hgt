module Utils (
  Point(..)
, Rect(..)

, on
) where

data Point a = Point {
                x :: a
              , y :: a
              } deriving (Show, Eq, Ord)

data Rect a = Rect { topLeft       :: Point a
                   , bottomRight   :: Point  a
                   } deriving (Show, Eq)


-- from http://anton-k.github.io/ru-haskell-book/book/5.html
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y
