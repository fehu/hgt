module TileRenderer (
  Point(..)
, Camera(..)
, MapRenderer, TileRenderer

, mkMapRenderer
, glFloat, int2GLfloat
, glInt
, rgb2GLcolor
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

mkMapRenderer :: Num a =>
                   IO()
                -> IO()
                -> (Tile id tpe state content -> IO())
                -> (Tiles id tpe state content -> Camera a -> [Tile id tpe state content])
                -> ((a, a) -> TileRenderer id tpe state content)
                -> Camera a
                -> MapRenderer id tpe state content
mkMapRenderer before after beforeRender visibles render camera mapRef = [before, renderers, after]
            where renderers = do Tiles.Map theTiles _  <- readIORef mapRef
                                 let toRender = visibles theTiles camera
                                 let s q = (q . bottomRight $ camera) - (q . topLeft $ camera) + 1
                                 let size = (s x, s y)
                                 let rendSeq = forM toRender $ \t -> do
                                        tRef <- newIORef t
                                        return $ beforeRender t : render size tRef
                                 let rendSeq' = fmap transpose rendSeq
                                 ioLayers <- fmap (map sequence_) rendSeq'
--                                 let x = ioLayers :: [IO ()]
                                 sequence_ ioLayers


glFloat :: Float -> GLfloat
glFloat f = fromRational $ toRational f

int2GLfloat :: Int -> GLfloat
int2GLfloat = glFloat . int2Float

glInt :: Int -> GLint
glInt i = fromInteger $ toInteger i

rgb2GLcolor :: Int -> Int -> Int -> Color3 GLfloat
rgb2GLcolor r g b = Color3 (f r) (f g) (f b)
                 where f c = glFloat $ (int2Float c) / 255
