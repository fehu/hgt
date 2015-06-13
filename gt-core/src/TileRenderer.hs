module TileRenderer (
  Point(..)
, Camera(..)
, MapRenderer, TileRenderer

, mkMapRenderer
, cameraSize

) where

import OpenGLRenderer
import Tiles

import Data.IORef
import Data.List
import Control.Monad( forM, mapM )
import Data.Traversable( for )

import GHC.Float.RealFracMethods

import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.UI.GLUT (GLfloat, GLint, Color3(..), DisplayCallback)


type MapRenderer  id tpe state content = Renderer (Tiles.Map id tpe state content)
type TileRenderer id tpe state content = Renderer (Tile      id tpe state content)

data Point a = Point {
                x :: a
              , y :: a
              } deriving (Show, Eq, Ord)

data Camera a = Camera{
                  topLeft       :: Point a
                , bottomRight   :: Point  a
                } deriving Show

cameraSize :: (Num a) =>  Camera a -> (a, a)
cameraSize camera = (s x, s y)
              where s q = (q . bottomRight $ camera) - (q . topLeft $ camera) + 1



mkMapRenderer :: (Num a) =>
                   IO()
                -> IO()
                -> (Tile id tpe state content -> IO())
                -> (Tiles id tpe state content -> Camera a -> [Tile id tpe state content])
                -> (Camera a -> TileRenderer id tpe state content)
                -> IORef(Camera a)
                -> MapRenderer id tpe state content
mkMapRenderer before after beforeRender visibles render cameraRef mapRef = [before, renderers, after]
            where renderers = do Tiles.Map theTiles _  <- readIORef mapRef
                                 camera <- readIORef cameraRef
                                 let toRender = visibles theTiles camera
                                 let rendSeq = forM toRender $ \t -> do
                                        tRef <- newIORef t
                                        return $ beforeRender t : render camera tRef
                                 let rendSeq' = fmap transpose rendSeq
                                 ioLayers <- fmap (map sequence_) rendSeq'
                                 sequence_ ioLayers

