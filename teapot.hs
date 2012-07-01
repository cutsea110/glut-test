{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, handle)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Graphics.UI.GLUT
import System.Exit (exitSuccess)

main :: IO ()
main = handle (\(ex :: SomeException) -> return ()) $ do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  initialWindowSize $= Size 320 240
  createWindow progName
  t <- now
  displayCallback $= display t
  reshapeCallback $= Just reshape
  -- メンドイので全部先に指定してしまう...
  depthFunc $= Just Less
  cullFace $= Just Back
  frontFace $= CW
  -- 光源を決める...
  light (Light 0) $= Enabled
  ambient  (Light 0) $= Color4 0.2 0.2 0.2 1
  diffuse  (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  position (Light 0) $= Vertex4 radiusf radiusf radiusf 0
  -- 素材を決める...
  materialAmbient Front $= Color4 0.25 0 0 1
  materialDiffuse Front $= Color4 1 0 0 1
  -- 法線は勝手に計算して欲しい...
  normalize $= Enabled
  rescaleNormal $= Enabled
  addTimerCallback 50 refresh
  mainLoop

refresh :: TimerCallback
refresh = postRedisplay Nothing >> addTimerCallback 50 refresh

now :: IO GLdouble
now = (/ 1000) . fromIntegral . (`rem` 360000) <$> get elapsedTime

angle :: IO GLdouble
angle = do
  let d = 2 * pi
  (_, r) <- properFraction . (/ d) . (/ 10) <$> now
  return $! d * r

radius :: GLdouble
radius = 5

radiusf :: GLfloat
radiusf = 5

display :: GLdouble -> DisplayCallback
display start = do
  clear [ColorBuffer, DepthBuffer]
  -- 適当に回転させて表示
  (!x, !z) <- (cos &&& sin) . (* radius) <$> angle
  loadIdentity
  lookAt (Vertex3 x 0 z) (Vertex3 0 0 0) (Vector3 0 1 0)
  lighting $= Enabled
  renderObject Solid (Teapot $ radius / 2)
  lighting $= Disabled
  swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho (-radius) radius (-radius) radius ((-radius) * 2.5) (radius * 2.5)
  matrixMode $= Modelview 0

