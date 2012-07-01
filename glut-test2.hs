module Main where

import Graphics.UI.GLUT hiding (position)
import Graphics.Rendering.OpenGL.GLU

import System.Environment
import System.Exit
import System.Random
import Data.IORef
import Control.Arrow
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

--タイマの間隔
timerInterval = 16

type Point= Vertex3 GLdouble

data GameObject =
        Particle{position::Point, velocity::Point}
    |    Player{position::Point, velocity::Point}

data GameState = Game{
    player::GameObject,
    objects::[GameObject],
    keys::IORef (Set.Set Key)}
data GameRenderer=Renderer{rendererFunc::DisplayCallback, game::IORef GameState}

addVer3::Num a => Vertex3 a->Vertex3 a->Vertex3 a
addVer3 (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2)
            = Vertex3 (x1+x2) (y1+y2) (z1+z2)
subVer3::Num a => Vertex3 a->Vertex3 a->Vertex3 a
subVer3 (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2)
            = Vertex3 (x1-x2) (y1-y2) (z1-z2)
            
updateObject::GameObject->GameObject
updateObject Particle{position=pos, velocity=vel}
    =Particle{
        position = addVer3 pos vel,
        velocity = vel}-- 重力ON addVer3 vel (Vertex3 0.0 (-0.001) 0.0) }

            
updateObjects::[GameObject]->[GameObject]
updateObjects os
    = map updateObject os 
    -- filter (\o-> getY (position o) >(-0.5))    --ある程度落ちた点は消す
    -- $ map updateObject os                    --座標を更新する
    where
        getY (Vertex3 _ y _) = y
    
newParticle::Point->Point->IO GameObject
newParticle p v=do
    newVel <- getRandomVel
    return Particle{
        position=p,
        velocity=addVer3 v newVel}
    where
        getRandomVel=do
            gx <- newStdGen
            (x,gy) <- return $ randomR ( -0.003, 0.003) gx
            (y,gz) <- return $ randomR ( -0.003, 0.003) gy
            (z,g') <- return $ randomR ( -0.003, 0.003) gz
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
  
--個数　場所　方向
newParticles::Int->Point->Point->IO [GameObject]
newParticles 0 _ _ = return []
newParticles n p v= do
    new <- newParticle p v
    ps<- newParticles (n-1) p v
    return (new:ps)
    
main = do
  
  prog <- getProgName
  args <- getArgs
  
  --Gameを作る
  keyState <- newIORef Set.empty
  gameState <- newIORef Game{ player=Player { position=Vertex3 0 0 0
                                            , velocity=Vertex3 0 0 0
                                            }
                            , objects=[]
                            , keys=keyState
                            }
  gameRenderer <- newIORef Renderer{ rendererFunc=display gameState
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
  reshapeCallback $= Just (reshape gameState)
    
  --キーボードやマウスのコールバック
  keyboardMouseCallback $= Just (keyboardProc keyState)
    
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
    
    setCenter gs
    
    pointSize $=4.0
    --表示
    showParticles    $ objects gs
    showPlayer        $ player gs
    
    --バッファの入れ替え
    swapBuffers
    
    where
        showParticles os = preservingMatrix $ do
            renderPrimitive Points $ mapM_
                vertex$ map position os
            
        showPlayer Player{position=Vertex3 x y z, velocity=_} =
            preservingMatrix $ do
                translate (Vector3 x y z)
                renderObject Wireframe (Sphere' 0.05 5 5)--(Teapot 0.06)

modifyGame::IORef GameState->IO()
modifyGame game = do
    gs <- readIORef game
    ks <- readIORef $ keys gs
    --プレイヤーを動かす
    ppos <- return $ position $ player gs
    pvel <- return $ velocity $ player gs
    addVecList    <- return $ map (`searchMap` vectorMap) $ Set.elems ks 
    addVec        <- return $ foldl (addVer3) (Vertex3 0 0 0) addVecList
    --プレイヤーが動くならパーティクルを吐く
    Vertex3 x y z <- return addVec
    ps <-if or [null addVecList, addVec==Vertex3 0 0 0]
        then return [] 
        else newParticles 32 (addVer3 ppos pvel) (addVer3 pvel (Vertex3 (-x) (-y) (-z)))
    newPlayer <- return Player{
        position=addVer3 ppos pvel,
        velocity=addVer3 pvel addVec
    }
            
    writeIORef game 
        Game{
            player    =newPlayer,
            objects    =take 10240 $ ps++(updateObjects$objects gs),
            keys    =keys gs}
    where
        searchMap f ((ch,v):xs)= if f==ch then v else searchMap f xs
        searchMap f []=Vertex3 0 0 0
        vectorMap = [
            (SpecialKey KeyLeft,    Vertex3 (-0.003) 0 0),
            (SpecialKey KeyRight,    Vertex3 0.003 0 0),
            (SpecialKey KeyUp,        Vertex3 0 0.003 0),
            (SpecialKey KeyDown,    Vertex3 0 (-0.003) 0)]
                

timerProc::IORef GameRenderer->IO ()
timerProc grRef = do
    gr <- readIORef grRef
    modifyGame $ game gr
    gs <- readIORef $ game gr
    rendererFunc gr
    addTimerCallback timerInterval $ timerProc grRef

setCenter gs = do
    Vertex3 x y z <- return $ position$player gs
    lookAt (Vertex3 0.0 0.0 (1.0)) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    --lookAt (Vertex3 x y (1.0)) (Vertex3 x y 0.0) (Vector3 0.0 1.0 0.0)

--ウィンドウのサイズが変更された時の処理
reshape gameState size@(Size w h)=do
    viewport $= (Position 0 0, size) --ウィンドウ全体を使う
    gs <- readIORef gameState
    --ビューボリュームの設定
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
    
    --少し後ろから撮影
    setCenter gs
    matrixMode $= Modelview 0
      
--キー入力の処理
keyboardProc keySet ch state _ _
    | ch     == Char 'q'    = exitWith ExitSuccess        --qが押されたら終了
    | state    == Down        = modifyIORef keySet (Set.insert ch)
    | state    == Up        = modifyIORef keySet (Set.delete ch)
    | otherwise            = return ()

