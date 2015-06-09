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

translateShift vsize tile = (c x, c y)
                   where c s = glFloat . int2Float . s . Tiles.id $ tile

--translateShift vsize tile = (c x / int2GLfloat (fst vsize - 1), c y / int2GLfloat (snd vsize - 1))
--                   where c s = glFloat . int2Float . s . Tiles.id $ tile

scaleRatio vsize = 1 / int2GLfloat (max (fst vsize) (snd vsize))

prepareToDrawPrimitives vsize tile = do loadIdentity
                                        let sc = scaleRatio vsize
                                        let (cx, cy) = translateShift vsize tile
                                        translate $ Vector3 (cx * sc) (cy * sc) 0
                                        putStrLn $ "vsize = " ++ show vsize
                                        putStrLn $ "translate: " ++ show (cx * sc)  ++ ", " ++ show (cy * sc)
                                        let sc2 = sc / 2
                                        putStrLn $ "scaleRatio = " ++ show sc2
                                        scale sc2 sc2 (1 :: GLfloat)

prepareToDrawText vsize tile pos = do loadIdentity
                                      let (cx, cy) = translateShift vsize tile
                                      let x = cx + fst pos
                                      let y = cy + snd pos
                                      translate $ Vector3 (x - 1) (y - 1) 0
                                      putStrLn $ "translate txt  : " ++ show x  ++ ", " ++ show y
                                      let sc = 1.3e-3 * scaleRatio vsize
                                      scale sc sc (sc :: GLfloat)


renderTile :: (Int, Int) -> TestTileRenderer
renderTile vsize tileRef = [primitives] -- , text
        where primitives = do tile <- readIORef tileRef
                              putStrLn $ "rendering primitives " ++ show tile
                              prepareToDrawPrimitives vsize tile
                              renderPrimitive Quads $ do
                                  let (r, g, b) = tileColor tile
                                  color $ rgb2GLcolor r g b
                                  tileQuad z -- (0.5 :: GLfloat)
              text       = do tile <- readIORef tileRef
                              prepareToDrawText vsize tile (0.25, 0.8)
                              color $ rgb2GLcolor 255 255 255
--                              scale sc sc (sc :: GLfloat)
                              -- Weather
                              putStrLn $ "render string" ++ show tile
                              renderString Roman . show . snd . state $ tile
              z  = glFloat 1
--              sc = 1.3e-3


renderMap :: IO() -> IO() -> Camera Int -> TestMapRenderer
renderMap before after = mkMapRenderer before after beforeRender getVisibles renderTile





