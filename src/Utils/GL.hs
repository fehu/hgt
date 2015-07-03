module Utils.GL(
  glFloat
, int2GLfloat
, glInt
, rgb2GLcolor
) where

import Graphics.UI.GLUT
import GHC.Float.RealFracMethods

glFloat :: Float -> GLfloat
glFloat f = fromRational $ toRational f

int2GLfloat :: Int -> GLfloat
int2GLfloat = glFloat . int2Float

glInt :: Int -> GLint
glInt i = fromInteger $ toInteger i

rgb2GLcolor :: Int -> Int -> Int -> Color3 GLfloat
rgb2GLcolor r g b = Color3 (f r) (f g) (f b)
                 where f c = glFloat $ (int2Float c) / 255
