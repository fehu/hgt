module TileRenderer (
  Point(..)
, Camera(..)
, MapRenderer, TileRenderer

, mkMapRenderer
) where

import OpenGLRenderer
import Tiles
import Data.IORef
import Control.Monad( foldM )
import Data.Traversable( for )



type MapRenderer  id tpe state content = Renderer (Tiles.Map id tpe state content)
type TileRenderer id tpe state content = Tile id tpe state content -> IO()

data Point a = Point {
                x :: a
              , y :: a
              } deriving Show

data Camera a = Camera{
                  topLeft       :: Point a
                , bottomRight   :: Point a
                } deriving Show

mkMapRenderer :: (Tiles id tpe state content -> Camera a -> [Tile id tpe state content])
                -> TileRenderer id tpe state content
                -> Camera a
                -> MapRenderer id tpe state content
mkMapRenderer visibles tr camera =
    \mapRef -> do Tiles.Map theTiles _  <- readIORef mapRef
                  let toRender  = visibles theTiles camera
                  let renderers = fmap tr toRender
                  sequence_ renderers
