import Graphics.UI.GLUT

import Data.IORef
import System.Environment

-- タイマの間隔
timerInterval :: Timeout
timerInterval = 40

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  
  -- 回転の角度を初期化
  rot <- newIORef 0.0
  
  -- GLUTの初期化
  initialize prog args
  initialDisplayMode $= [RGBMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
  
  -- ウィンドウを作る
  createWindow "guruGuru"
  
  -- 表示に使うコールバック関数の指定
  displayCallback $= display rot
  
  -- ウィンドウのサイズが変更された時に呼ぶコールバック関数の指定
  reshapeCallback $= Just reshape
  
  -- タイマを作る
  addTimerCallback timerInterval $ timerProc (display rot)
  
  -- GLUTのメインループに入る
  mainLoop

display :: IORef GLdouble -> IO ()
display rot = do
  -- 回転させる
  modifyIORef rot (+14.4)
  r <- readIORef rot
  
  -- 背景を黒にする
  clearColor $= Color4 0.0 0.0 0.0 0.0
  clear [ColorBuffer]
  
  -- 単位行列を読み込む
  loadIdentity
  
  -- 表示
  preservingMatrix $ do
    rotate r (Vector3 0.0 0.0 1.0 :: Vector3 GLdouble)
    renderPrimitive Quads $ mapM_ vertex [
      Vertex3 0.10 0.10 0.0,
      Vertex3 (-0.10) 0.10 0.0,
      Vertex3 (-0.10) (-0.10) 0.0,
      Vertex3 0.10 (-0.10) 0.0 :: Vertex3 GLfloat]
    -- バッファの入れ替え
    swapBuffers

-- タイマが呼ばれるたびにactを繰り返す
timerProc :: IO a -> IO ()
timerProc act = do
  act
  addTimerCallback timerInterval $ timerProc act

-- ウィンドウのサイズが変更された時の処理
reshape :: Size -> IO ()
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size) -- ウィンドウ全体を使う
  
  -- ビューボリュームの設定
  matrixMode $= Projection
  loadIdentity
  perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
  
  -- 少し後ろから撮影
  lookAt (Vertex3 0.0 0.0 (-1.0)) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  matrixMode $= Modelview 0
