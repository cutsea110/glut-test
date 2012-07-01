import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU

import System.Environment

main = do
  
  prog <- getProgName
  args <- getArgs
  
  --GLUTの初期化
  initialize prog args
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
    
  --ウィンドウを作る
  createWindow "renderString sample"
    
  --表示に使うコールバック関数の指定
  displayCallback $= display
    
  --ウィンドウのサイズが変更された時に呼ぶコールバック関数の指定
  reshapeCallback $= Just reshape

  --GLUTのメインループに入る
  mainLoop

display = do
    --背景を青にする
    clearColor $= Color4 0.0 0.0 1.0 0.0
    clear [ColorBuffer]
    
    --単位行列を読み込む
    loadIdentity
    
    --表示
    lineWidth $= 4.0
    preservingMatrix $ do
        scale (0.001::GLdouble) 0.001 0.001
        w <- stringWidth Roman "Stroke font"
        translate (Vector3 (-0.5*(fromIntegral w)) 0 0 ::Vector3 GLfloat)
        renderString Roman "Stroke font"
        
    --バッファの入れ替え
    swapBuffers
    
--ウィンドウのサイズが変更された時の処理
reshape size@(Size w h)=do
    viewport $= (Position 0 0, size) --ウィンドウ全体を使う
    
    --ビューボリュームの設定
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
    
    --少し後ろから撮影
    lookAt (Vertex3 0.0 0.0 1.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0

