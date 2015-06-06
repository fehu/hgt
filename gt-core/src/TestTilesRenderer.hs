module TestTilesRenderer (
  TestTileRenderer
, TestMapRenderer

, renderTile
, renderMap

, glFloat, int2GLfloat
, glInt
, rgb2GLcolor
) where

import OpenGLRenderer
import Tiles
import TestTiles as TT
import TileRenderer as TR

import Data.IORef
import Data.Map
import Control.Monad
import GHC.Float.RealFracMethods

import Graphics.UI.GLUT


type TestTileRenderer = Renderer TT.Tile
type TestMapRenderer  = Renderer TT.Map

getVisibles :: TT.Tiles -> Camera Int -> [TT.Tile]
getVisibles tiles camera = elems $ filterWithKey inRange tiles
                        where inRange = \(TR.Point xx yy) _ ->      xx >= x tl && xx <= x br
                                                                 && yy >= y tl && yy <= y br
                              tl = topLeft camera
                              br = bottomRight camera

beforeRender :: TT.Tile -> IO()
beforeRender t = do let c s = glFloat . int2Float . s . Tiles.id $ t
                    let cx = c x
                    let cy = c y

                    loadIdentity
                    translate $ Vector3 cx cy 0
                    putStrLn $ "translate: " ++ show cx  ++ ", " ++ show cy

tileQuad z = do vertex $ Vertex3 (-1) (-1) z
                vertex $ Vertex3 1    (-1) z
                vertex $ Vertex3 1    1    z
                vertex $ Vertex3 (-1) 1    z

renderTile :: TestTileRenderer
renderTile tileRef = [primitives, text]
        where primitives = do tile <- readIORef tileRef
                              putStrLn $ "rendering primitives " ++ show tile
                              renderPrimitive Quads $ do
                                  let (r, g, b) = case tpe tile of Plain       -> (0, 255, 0)
                                                                   Hill        -> (255, 255, 0)
                                                                   Mountain    -> (139, 69, 19)
                                                                   Sea         -> (0, 0, 255)
                                  color $ rgb2GLcolor r g b
                                  tileQuad z
              text       = do Tiles.Tile id tpe state content <- readIORef tileRef
                              loadIdentity
                              scale sc sc (sc :: GLfloat)
                              color $ rgb2GLcolor 0 0 0
                              -- Weather
                              renderString Roman $ show $ snd state

              z  = glFloat 1
              sc = 1.3e-3


renderMap :: IO() -> IO() -> Camera Int -> TestMapRenderer
renderMap before after = mkMapRenderer before after beforeRender getVisibles renderTile





