module OpenGLRenderer (
  run
, Renderer
, Mutator
) where

import Data.IORef
import Graphics.UI.GLUT


type Renderer w = IORef w -> DisplayCallback
type Mutator  w = IORef w -> IO()

-- from https://mail.haskell.org/pipermail/beginners/2010-November/005709.html

run :: Renderer w -> Maybe (Mutator w) -> Maybe ReshapeCallback -> w -> IO()
run display change reshaped world = do  getArgsAndInitialize
                                        _window <- createWindow "Test"

                                        worldM <- newIORef world

                                        reshapeCallback $= reshaped
                                        displayCallback $= (display worldM)
                                        idleCallback $= fmap ($ worldM) change

                                        mainLoop

