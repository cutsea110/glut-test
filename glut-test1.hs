module Main where

import Graphics.UI.GLUT hiding (position)
import Graphics.Rendering.OpenGL.GLU

import Control.Arrow
import System.Environment
import System.Exit
import System.Random
import Data.IORef

--タイマの間隔
timerInterval = 16

type Point= Vertex3 GLdouble
data GameObject = Particle{position::Point, velocity::Point}
data GameState = Game{objects::[GameObject]}
data GameRenderer=Renderer{rendererFunc::DisplayCallback, game::IORef GameState}

updateObject::GameObject->GameObject
updateObject Particle{position=pos, velocity=vel}
    =Particle{
        position = addVer3 pos vel,
        velocity = addVer3 vel (Vertex3 0.0 (-0.001) 0.0) }
    where
        addVer3 (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2)
            = Vertex3 (x1+x2) (y1+y2) (z1+z2)
            
updateObjects::[GameObject]->[GameObject]
updateObjects os
    = filter (\o-> getY (position o) >(-0.5))    --ある程度落ちた点は消す
        $ map updateObject os                    --座標を更新する
    where
        getY (Vertex3 _ y _) = y
        
newParticle::IO GameObject
newParticle=do
    newVel <- getRandomVel
    return Particle{
        position=Vertex3 0 0 0,
        velocity=newVel}
    where
        getRandomVel::IO Point
        getRandomVel=do
            gx <- newStdGen
            (x,gy) <- return $ randomR ( 0.01, 0.02) gx
            (y,gz) <- return $ randomR ( 0.01, 0.03) gy
            (z,g') <- return (0.0, gz) -- $ randomR (-0.02, 0.02) gz
            return $ Vertex3 x y z

-- ref. import f2f and instance declaration for Random GLdouble
-- from rsagl package RSAGL.Types module
{-# RULES
"f2f/id" f2f = id #-}
f2f :: (RealFloat a, RealFloat b) => a -> b
f2f = uncurry encodeFloat . decodeFloat
{-# INLINE f2f #-}

instance Random GLdouble where
  randomR (lo, hi) g = first f2f $ randomR (f2f lo, f2f hi :: Double) g
  random g = first (f2f :: Double -> GLdouble) $ random g
  

newParticles::Int->IO [GameObject]
newParticles 0 = return []
newParticles n = do
    p <- newParticle
    ps<- newParticles $ n-1 
    return $ p:ps
    
main = do
  
  prog <- getProgName
  args <- getArgs
  
  --Gameを作る
  p <- newParticle
  gameState <- newIORef Game{objects=[p]}
  gameRenderer <- newIORef Renderer { rendererFunc=display gameState
                                      , game=gameState
                                      }
  --GLUTの初期化
  initialize prog args
  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize $= Size 640 480
    
  --ウィンドウを作る
  createWindow "Particle"
    
  --表示に使うコールバック関数の指定
  displayCallback $= display gameState-- gameState
    
  --ウィンドウのサイズが変更された時に呼ぶコールバック関数の指定
  reshapeCallback $= Just reshape
    
  --キーボードやマウスのコールバック
  keyboardMouseCallback $= Just (keyboardProc)
    
  --タイマを作る
  addTimerCallback timerInterval $ timerProc gameRenderer
    
  --GLUTのメインループに入る
  mainLoop

display gameState= do
    --背景を黒にする
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clear [ColorBuffer]
    
    --単位行列を読み込む
    loadIdentity
    
    gs <- readIORef gameState
    pointSize $=4.0
    --表示
    preservingMatrix $ do
        renderPrimitive Points $ mapM_
            vertex$ map position (objects gs)
    --バッファの入れ替え
    swapBuffers

--タイマが呼ばれるたびにactを繰り返す
timerProc::IORef GameRenderer->IO ()
timerProc grRef = do
    gr <- readIORef grRef
    gs <- readIORef $ game gr
    rendererFunc gr
    ps <-newParticles 32
    writeIORef (game gr)
        Game{
            objects=ps++(updateObjects$objects gs)}
    addTimerCallback timerInterval $ timerProc grRef
    
--ウィンドウのサイズが変更された時の処理
reshape size@(Size w h)=do
    viewport $= (Position 0 0, size) --ウィンドウ全体を使う
    
    --ビューボリュームの設定
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
    
    --少し後ろから撮影
    lookAt (Vertex3 0.5 0.2 (1.0)) (Vertex3 0.5 0.2 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0
    
--キー入力の処理
keyboardProc ch state _ _
    | ch     == Char 'q'    = exitWith ExitSuccess        --qが押されたら終了
--    | state    == Down        = modifyIORef arg (*(-1))    --それ以外なら回転の方向を変える
    | otherwise            = return ()
