{-# LANGUAGE ScopedTypeVariables, 
             FlexibleContexts#-} 
{-  
  2012 Joel Svensson   

  DungeonGame
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


----------------------------------------------------------------------------
-- Constants 

walkSpeed      = 32 

data ViewDirection = 
  North | East | South | West
  deriving (Eq, Show, Enum) 

direction :: ViewDirection -> Float 
direction North = 0 
direction West  = pi/2
direction South = pi 
direction East  = 3*pi/2 

    

----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
    
  -- 300 ~ 70 deg view 
  -- 350 ~ 65 
  -- 450 ~ 60 
  let vc = mkViewConfig 300 -- (floor (800.0*0.6)) -- viewDist  
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

  
  monster <- conv pf =<< loadBMP "../../Data/Eye1.bmp"  
  let monsterSprite = Sprite (mkPoint (5*256+128,1*256+128))
                              0
                              (256,256) 
                              monster
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite (mkPoint (4096,4096))
    (North,mkPoint (255+127,255+127))
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
             -> (ViewDirection,Point2D) 
             -> Int32
             -> MapType
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster targ (dir,pos) ly level = do 
  
  let (x,y) = (point2DGetX pos, point2DGetY pos)
      lights = ([mkLight (x,y) (1.0,1.0,1.0)] ++ 
                [mkLight ((i+5)*256+128,(j+1)*256+128+(fromIntegral ly)) (1.0,0.0,0.0) 
                | i <- [0,9], j <- [0]])
               
  withLights lights $ \lights' ->              
    do 
       
       sl <- renderView vc level lights' (pos,direction dir) wallTextures screen
                         
       let dists  = map sliceDistance sl
       let monsterTfrmd = viewTransformSprite vc (pos,direction dir) monster
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
  
  -- process events 
  e <- pollEvent
  
  let (pos',dir',quit) = 
        case e of 
          (KeyDown k) -> 
            case symKey k of 
              SDLK_LEFT -> (pos,moveLeft dir,False)
              SDLK_RIGHT -> (pos,moveRight dir,False) 
              SDLK_UP -> (moveForward dir pos, dir,False)
              SDLK_DOWN -> (moveBackward dir pos, dir,False) 
              SDLK_ESCAPE -> (pos,dir,True)
              otherwise -> (pos,dir,False) 
          Quit -> (pos,dir,True)
          otherwise -> (pos,dir,False)
          

  unless quit $ eventLoop vc screen floorTextures wallTextures monster' targ' (dir',pos') ((ly + 128) `mod` 4096) level   
  
  
  -- very crude colision against walls added
  where 
    moveRight :: ViewDirection -> ViewDirection
    moveRight North = East 
    moveRight East  = South
    moveRight South = West 
    moveRight West  = North 
    
    moveLeft :: ViewDirection -> ViewDirection 
    moveLeft North = West 
    moveLeft West  = South 
    moveLeft South = East 
    moveLeft East  = North 
    
    -- TODO: Use grid coords instead of "fine"-coords. 
    moveForward :: ViewDirection -> Point2D -> Point2D 
    moveForward North (Point2D x y) = mkPoint (x,y+256) 
    moveForward East  (Point2D x y) = mkPoint (x+256,y)
    moveForward South (Point2D x y) = mkPoint (x,y-256) 
    moveForward West  (Point2D x y) = mkPoint (x-256,y) 
   
    
    moveBackward :: ViewDirection -> Point2D -> Point2D 
    moveBackward North (Point2D x y) = mkPoint (x,y-256) 
    moveBackward East  (Point2D x y) = mkPoint (x-256,y)
    moveBackward South (Point2D x y) = mkPoint (x,y+256) 
    moveBackward West  (Point2D x y) = mkPoint (x+256,y) 
   
    
     