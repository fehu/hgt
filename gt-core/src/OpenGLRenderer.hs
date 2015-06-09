module OpenGLRenderer (
  run
, Renderer
, Mutator

, Callbacks(..)
) where

import Data.IORef
import Graphics.UI.GLUT


type Renderer w = IORef w -> [DisplayCallback]
type Mutator  w = IORef w -> IO()

-- from https://mail.haskell.org/pipermail/beginners/2010-November/005709.html


data Callbacks = Callbacks { callReshape :: Maybe (Window -> ReshapeCallback)
                           , callKey     :: Maybe (Window -> KeyboardCallback)
                           , callSpecKey :: Maybe (Window -> SpecialCallback)
                           }


run :: Renderer w -> Maybe (Mutator w) -> Callbacks -> w -> IO()
run display change callbacks world = do getArgsAndInitialize
                                        _window <- createWindow "Test"

                                        worldM <- newIORef world

                                        reshapeCallback $= (fmap ($ _window) $ callReshape callbacks)
                                        displayCallback $= sequence_ (display worldM)
                                        idleCallback $= fmap ($ worldM) change

                                        keyboardCallback $= (fmap ($ _window) $ callKey callbacks)
                                        specialCallback  $= (fmap ($ _window) $ callSpecKey callbacks)

                                        mainLoop

