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
beforeRender t = putStrLn "todo: not used"

tileQuad z = do vertex $ Vertex3 (-1) (-1) z
                vertex $ Vertex3 1    (-1) z
                vertex $ Vertex3 1    1    z
                vertex $ Vertex3 (-1) 1    z

tileColor tile = case tpe tile of Plain       -> (0, 255, 0)
                                  Hill        -> (255, 255, 0)
                                  Mountain    -> (139, 69, 19)
                                  Sea         -> (0, 0, 255)

translateShift tile = (c x, c y)
                   where c s = glFloat . int2Float . s . Tiles.id $ tile

translatePrimitives tile = do let (cx, cy) = translateShift tile
                              loadIdentity
                              translate $ Vector3 cx cy 0
                              putStrLn $ "translate: " ++ show cx  ++ ", " ++ show cy

translateText tile pos = do let (cx, cy) = translateShift tile
                            let x = cx + fst pos
                            let y = cy + snd pos
                            loadIdentity
                            translate $ Vector3 (x - 1) (y - 1) 0
--                            rasterPos $ Vertex2 x y
                            putStrLn $ "translate txt  : " ++ show x  ++ ", " ++ show y

renderTile :: TestTileRenderer
renderTile tileRef = [primitives, text]
        where primitives = do tile <- readIORef tileRef
                              putStrLn $ "rendering primitives " ++ show tile
                              translatePrimitives tile
                              renderPrimitive Quads $ do
                                  let (r, g, b) = tileColor tile
                                  color $ rgb2GLcolor r g b
                                  tileQuad z
              text       = do tile <- readIORef tileRef
                              color $ rgb2GLcolor 0 0 0
                              translateText tile (0.25, 0.8)
                              scale sc sc (sc :: GLfloat)
                              -- Weather
                              putStrLn $ "render string" ++ show tile
                              renderString Roman . show . snd . state $ tile
              z  = glFloat 1
              sc = 1.3e-3


renderMap :: IO() -> IO() -> Camera Int -> TestMapRenderer
renderMap before after = mkMapRenderer before after beforeRender getVisibles renderTile





