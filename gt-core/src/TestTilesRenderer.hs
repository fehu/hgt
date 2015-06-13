module TestTilesRenderer (
  TestTileRenderer
, TestMapRenderer

, renderTile
, renderMap

, tileQuad
) where

import OpenGLRenderer
import GLUtils
import Tiles
import TestTiles as TT
import TileRenderer as TR

import Data.IORef
import Data.Map
import Data.Maybe (fromMaybe)

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

scaleTranslate vsize xy = (sc * 2 * fst xy, sc * 2 * snd xy)
                       where sc = scaleRatio vsize

translateShift camera tile = scaleTranslate camera (shift x, shift y)
                   where shift q = c q - h (q . topLeft) - 1
                         c s = int2GLfloat . s . Tiles.id $ tile
                         h s = int2GLfloat . s $ camera


scaleRatio camera = 1 / int2GLfloat (max (fst vsize) (snd vsize))
                 where vsize = cameraSize camera

prepareToDrawPrimitives camera tile = do loadIdentity
                                         let (cx, cy) = translateShift camera tile
                                         translate $ Vector3 cx cy 0
                                         putStrLn $ "translate: " ++ show cx ++ ", " ++ show cy
                                         let sc = scaleRatio camera
                                         putStrLn $ "scaleRatio = " ++ show sc
                                         scale sc sc (1 :: GLfloat)

prepareToDrawText camera tile pos = do loadIdentity
                                       let (cx, cy) = translateShift camera tile
                                       let spos = scaleTranslate camera pos
                                       let x = cx + (fst spos / 2)
                                       let y = cy + (snd spos / 2)
                                       translate $ Vector3 (x - 0) (y - 0) 0
                                       putStrLn $ "translate txt  : " ++ show x  ++ ", " ++ show y
                                       let sc = 2e-3 * scaleRatio camera
                                       scale sc sc (sc :: GLfloat)

renderTile :: Camera Int -> TestTileRenderer
renderTile camera tileRef = [primitives, text]
        where primitives = do tile <- readIORef tileRef
                              putStrLn $ "rendering primitives " ++ show tile
                              prepareToDrawPrimitives camera tile
                              renderPrimitive Quads $ do
                                  let (r, g, b) = tileColor tile
                                  color $ rgb2GLcolor r g b
                                  tileQuad z
                              renderPrimitive LineLoop $ do
                                  color $ rgb2GLcolor 0 0 0
                                  tileQuad z
              text       = do tile <- readIORef tileRef
                              color $ rgb2GLcolor 0 0 0 -- 255 255 255
                              -- Weather
                              prepareToDrawText camera tile (0.25, 0.5)
                              putStrLn $ "render string" ++ show tile
                              renderString Roman . show . snd . state $ tile
                              -- Owner
                              prepareToDrawText camera tile (-0.9, 0)
                              let ownerOpt = fmap renderOwner (fst .state $ tile)
                                          where renderOwner = renderString Roman . TT.name
                              fromMaybe (return ()) ownerOpt
              z  = glFloat 1


renderMap :: IO() -> IO() -> IORef(Camera Int) -> TestMapRenderer
renderMap before after = mkMapRenderer before after beforeRender getVisibles renderTile





