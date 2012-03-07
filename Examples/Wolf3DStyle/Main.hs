{-# LANGUAGE ScopedTypeVariables, 
             FlexibleContexts#-} 
{-  
  2012 Joel Svensson   


  RayCasting (Like in the old Wolfenstein 3d game)  

  This is an attempt to Haskellify the code from 
  Christopher Lamptons Book "Gardens of Imagination"  
   

  **************************************
  This is also an exercise in using SDL. 
  
  Early problems in using SDL are 
    - SDL is low level! 
       It expects you to implement your own graphics routines 
       on the supplied "surfaces" and pixels thereof. This is Ok 
       in C where it possible to write your own optimized drawing routines 
       but I suspect that it is, from that point of view,  trickier in Haskell. 
       + If SDL had optimized "larger" building blocks like stretchBlit it would help. 
         
-} 

module Main where 

import Prelude hiding ((!!))
import qualified Prelude as P
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types


import Engine.Math
import Engine.Render
import Engine.RItem
import Engine.Sprite 
import Engine.ZBuffer
import Engine.Light 
import Engine.ViewConfig
import Engine.Slice
import Engine.World

import Engine.CubeWorld.Map
import Engine.CubeWorld.RayCast
import Engine.CubeWorld.Render

import Control.Monad
import Data.Array
import Data.Array.Storable
import Data.Maybe

import Data.Word
import Data.Bits
import Data.Int

import SDLUtils 

import Foreign.Ptr
import Foreign.Storable 
import Foreign.Marshal.Array
import Foreign.C.Types

import CExtras
import MathExtras

import System.IO.Unsafe
----------------------------------------------------------------------------
--
    
testLevel = [[1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1], 
             [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,1,0,0,0,0,0,2,1,2,1,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,2,0,0,0,1],
             [1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1],
             [1,0,1,0,1,2,0,0,0,0,0,2,0,0,0,1],
             [1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,1],
             [1,0,2,0,0,2,0,0,1,2,1,2,0,0,0,1],
             [1,0,2,2,1,1,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1]] 
            
testFloor = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --0
             [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --1
             [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --2
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --3
             [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --4
             [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --5
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --6
             [0,1,0,0,0,0,0,0,0,0,3,0,0,0,0,0], --7
             [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --8
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --9
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --10
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --11
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --12
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --13
             [0,3,0,0,0,0,0,0,0,0,0,0,0,0,3,0], --14
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]  --15
            ]

--testLevelArr :: MapType
--testLevelArr = unsafePerformIO$ listMap (16,16) testLevel

--testLevelFloorArr :: MapType 
--testLevelFloorArr = unsafePerformIO$ listMap (16,16) testFloor


----------------------------------------------------------------------------
-- Constants 

walkSpeed      = 32 

    
----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
    
  let vc = mkViewConfig (floori_ (800.0*0.6)) -- viewDist  
                        128    -- viewHeight
                        800    -- windowWidth
                        600    -- windowHeight
                        (256,256)  -- wall dimensions
    
  setVideoMode 800 600 32 []

  
  screen <- getVideoSurface
  -- toggleFullscreen screen
  

  -- disable tracking of mousemotion events 
  enableEvent SDLMouseMotion False
  
  let pf = surfaceGetPixelFormat screen
                       
  --wallTextures <- sequence [conv pf =<< loadBMP "Data/textureLarge1.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge2.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge1.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge1.bmp"]
                 
  -- These textures are not in the repo yet.          
  wallTextures <- sequence [conv pf =<< loadBMP "../../Data/Wall3.bmp"
                           ,conv pf =<< loadBMP "../../Data/Wall2.bmp"
                           ,conv pf =<< loadBMP "../../Data/Door.bmp"
                           ,conv pf =<< loadBMP "../../Data/DoorOpen.bmp"]
   
  floorTextures <- sequence [conv pf =<< loadBMP "../../Data/Floor1.bmp"
                            ,conv pf =<< loadBMP "../../Data/Floor2.bmp"
                            ,conv pf =<< loadBMP "../../Data/Floor3.bmp"
                            ,conv pf =<< loadBMP "../../Data/Brunn.bmp"]
  
  testLevelArr <- mkMap (16,16) testLevel testFloor testFloor wallTextures floorTextures floorTextures
  
  --floorTextures <- sequence [conv pf =<< loadBMP "Data/Floor.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor1.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor2.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor3.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor4.bmp" ]

  --floorTextures <- sequence [conv pf =<< loadBMP "Data/floor1.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor1.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor2.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor3.bmp"
  --                          ,conv pf =<< loadBMP "Data/floor4.bmp" ]

  
  monster <- conv pf =<< loadBMP "../../Data/Eye1.bmp"  
  let monsterSprite = Sprite (mkPoint (5*256+128,1*256+128))
                              0
                              (256,256) 
                              monster
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite (mkPoint (4096,4096))
    (False,False,False,False) -- Keyboard state
    (0.0,mkPoint (256+128 ,256+128))
    0
    testLevelArr
    
  quit
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: ViewConfig 
             -> Surface 
             -> [Surface] 
             -> [Surface] 
             -> Sprite -- [Sprite]
             -> Point2D
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Point2D) 
             -> Int32
             -> MapType
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster targ (up,down,left,right) (r,pos) ly testLevelArr = do 
  
  let (x,y) = (point2DGetX pos, point2DGetY pos)
      lights = ([mkLight (x,y) (1.0,1.0,1.0)] ++ 
                [mkLight ((i+5)*256+128,(j+1)*256+128+(fromIntegral ly)) (1.0,0.0,0.0) 
                | i <- [0,9], j <- [0]])
               
  withLights lights $ \lights' ->              
    do 
       sl <- renderView vc
                         testLevelArr 
                         lights' 
                         (pos,r) 
                         wallTextures 
                         screen
                         
       let dists  = map sliceDistance sl
         --  bots   = map sliceBot      sl 
       --newFloorCast3 vc testLevelFloorArr lights' bots (pos,r) floorTextures screen                                     

       
       let monsterTfrmd = viewTransformSprite vc (pos,r) monster
             --sortRItems $ 
             --catMaybes $ 
             --map (viewTransformSprite vc (pos,r)) monster    
       
       withZBuffer dists $ \zbuf -> maybe (return ()) (renderRItem screen zbuf lights') monsterTfrmd
--         case monsterTfrmd of 
 --          Nothing -> return () 
  --         (Just m) -> renderRItem screen zbuf lights' m
         --sequence_ $ map (renderRItem screen zbuf lights') monsterTfrmd
                    
                             
       
       
    
  SDL.flip screen
  
  -- process monster (needs a monster data type and monster "scripts"!) 
  
  let targ' = if (spritePos monster `distance` targ < 32) 
              then 
                if (targ == mkPoint (4096,4096)) 
                then mkPoint (0,0)
                else mkPoint (4096,4096)
              else targ
      (Point2D sx sy) = targ' - spritePos monster
      (Vector2D nx ny) = normalize (Vector2D sx sy)
      newPos = spritePos monster `translate` (Vector2D (nx*16) (ny*16)) 
      monster' = Sprite newPos (spriteElevation monster) (spriteDims monster) (spriteTexture monster)
      
  --putStrLn$ "position is: " ++ show (newPos)
  --putStrLn$ "target is: " ++ show targ
  
  
  
  -- process events 
  e <- pollEvent
  
  
  let (up',down',left',right',b) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT    -> (up,down,True,right,False)
              SDLK_RIGHT   -> (up,down,left,True,False)
              SDLK_UP      -> (True,down,left,right,False)
              SDLK_DOWN    -> (up,True,left,right,False)
              SDLK_ESCAPE  -> (up,down,left,right,True)
              otherwise    -> (up,down,left,right,False)
          (KeyUp k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> (up,down,False,right,False)
              SDLK_RIGHT -> (up,down,left,False,False)
              SDLK_UP    -> (False,down,left,right,False)
              SDLK_DOWN  -> (up,False,left,right,False)
              otherwise  -> (up,down,left,right,False)
          Quit -> (up,down,left,right,True) 
          otherwise -> (up,down,left,right,False)
  
  (r',x',y') <- (moveLeft left' >=> moveRight right' >=> moveUp up' >=> moveDown down') (r,x,y) 
  
  let pos' = mkPoint (x',y')
  unless b $ eventLoop vc screen floorTextures wallTextures monster' targ' (up',down',left',right') (r',pos') ((ly + 128) `mod` 4096) testLevelArr   
  
  
  -- very crude colision against walls added
  where 
    a = 3
    moveLeft :: Monad m => Bool -> (Float,Float,Float) -> m (Float,Float,Float)
    moveLeft  b (r,x,y) = return $ if b then (r+0.08,x,y) else (r,x,y) 
    moveRight :: Monad m => Bool -> (Float,Float,Float) -> m (Float,Float,Float)
    moveRight b (r,x,y) = return $ if b then (r-0.08,x,y) else (r,x,y) 
    
    moveUp :: (MArray StorableArray Int32 m,Monad m) => Bool -> (Float,Float,Float) -> m (Float,Float,Float)
    moveUp    b (r,x,y) = 
      do 
        ma <- movementAllowed (x',y')
        return $ if b && ma then (r,x',y')   else (r,x,y) 
      where 
        x' = x - ((fromIntegral walkSpeed)*sin r)
        y' = y + ((fromIntegral walkSpeed)*cos r)
    moveDown :: (MArray StorableArray Int32 m,Monad m) => Bool -> (Float,Float,Float) -> m (Float,Float,Float)
    moveDown  b (r,x,y) = 
      do 
        ma <- movementAllowed (x',y')
        return $ if b && ma  then (r,x',y')   else (r,x,y) 
      where 
        x' = x + ((fromIntegral walkSpeed)*sin r)
        y' = y - ((fromIntegral walkSpeed)*cos r)
        
    movementAllowed ::(MArray StorableArray Int32 m,  Monad m) => (Float,Float) -> m Bool    
    movementAllowed (px,py) = 
      do 
        value <- testLevelArr !! ((floor px) `div` wallWidth vc,(floor py) `div` wallWidth vc)
        return$ value == 0
     