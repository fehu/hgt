module Main ( main ) where

import OpenGLRenderer
import Utils
import Utils.GL
import Tiles
import Tiles.Renderer
import TestTiles as TT
import TestRenderer

import Data.Map (fromList)
import Data.IORef

import Graphics.UI.GLUT hiding (Point)

player1 = Owner "1" "xxPLAYERxx"
player2 = Owner "2" "__player__"


coords :: [TileId]
coords = [Point x y | x <- [0, 5], y <- [0, 1]]
--coords = do
--    x <- [0, 1]
--    y <- [0, 1]
--    return $ Coordinate x y
--coords = [0, 1] >>= (
--    \x -> [0, 1] >>= (
--        \y ->
--            return $ Coordinate x y
--        )
--    )

tls :: [TT.Tile]
tls = [
        Tile (Point 0 0) Sea       (Nothing,      Storm)   []
      , Tile (Point 0 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 1 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 1 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 2 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 2 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 3 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 3 1) Plain     (Just player1, Cloudy)  []
      , Tile (Point 4 0) Hill      (Just player2, Rain)    []
      , Tile (Point 4 1) Mountain  (Nothing,      Sunny)   []
      ]

neighbours :: Neighbours TileId
neighbours = fromList $ map f tls
          where f = \(Tile id _ _ _) -> (id, filter (id /=) coords)

type World = TT.Map

world :: World
world = Tiles.Map (mkTiles tls) Main.neighbours


reshaped ::  ReshapeCallback --Mutator World
reshaped size = do putStrLn $ "viewport"
                   viewport $= (mkPosition 0 0, mkSize 400 400) -- size
                   postRedisplay Nothing

mkPosition :: Int -> Int -> Position
mkPosition x y = Position (glInt x) (glInt y)

mkSize :: Int -> Int -> Size
mkSize w h = Size (glInt w) (glInt h)


keyCallbacks :: Maybe (Window -> IORef(Camera Int) -> KeyboardCallback)
keyCallbacks = Just $ \w -> \cRef -> \key pos -> case key of '\ESC' -> do destroyWindow w
                                                             _      -> return () --putStrLn $ "key " ++ show key

keySpecialCallbacks :: Maybe (Window -> IORef(Camera Int) -> SpecialCallback)
keySpecialCallbacks = Just $ \w -> \cRef -> \key pos -> case key of KeyUp    -> moveCamera cRef Main.Up
                                                                    KeyRight -> moveCamera cRef Main.Right
                                                                    KeyDown  -> moveCamera cRef Main.Down
                                                                    KeyLeft  -> moveCamera cRef Main.Left
                                                                    _        -> return ()

initialCamera = Rect { topLeft      = Point 0 0
                     , bottomRight  = Point 5 5
                     }

data Direction = Left | Right | Up | Down deriving Show

moveCamera :: IORef (Camera Int) -> Direction -> IO()
moveCamera cameraRef dir = do camera <- readIORef cameraRef
                              putStrLn $ "dir = " ++ show dir
                              let updCamera fx fy = Rect ptT ptB
                                                 where ffx g = fx . x . g $ camera
                                                       ffy g = fy . y . g $ camera
                                                       ptT = Point (ffx topLeft) (ffy topLeft)
                                                       ptB = Point (ffx bottomRight) (ffy bottomRight)
                              let id x = x
                              let minus1 x = x - 1
                              let plus1  x = x + 1

                              let nCamera = case dir of Main.Left  -> updCamera minus1 id
                                                        Main.Right -> updCamera plus1  id
                                                        Main.Up    -> updCamera id plus1
                                                        Main.Down  -> updCamera id minus1
                              writeIORef cameraRef nCamera
                              postRedisplay Nothing

cameraVar :: IO( IORef (Camera Int) )
cameraVar = newIORef initialCamera

render :: IORef (Camera Int) -> Renderer World
render cameraRef = do renderMap before after cameraRef
      where before = do clear [ColorBuffer]
                        loadIdentity
                        color $ rgb2GLcolor 255 255 255
                        renderPrimitive Quads $ tileQuad (1 :: GLfloat)
            after  = flush

callbacks = Callbacks{ callReshape = Nothing
                     , callKey = keyCallbacks
                     , callSpecKey = keySpecialCallbacks
                     }

main :: IO()
main = run cameraVar render Nothing callbacks world
