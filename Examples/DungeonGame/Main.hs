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

testLevelArr :: MapType
testLevelArr = unsafePerformIO$ listMap (16,16) testLevel

testLevelFloorArr :: MapType 
testLevelFloorArr = unsafePerformIO$ listMap (16,16) testFloor


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
-- Cast for floors 

newFloorCast3 :: ViewConfig 
                 -> MapType 
                 -> Lights 
                 -> [Int32]
                 -> View 
                 -> [Surface] 
                 -> Surface 
                 -> IO ()
newFloorCast3 vc world lights slices view textures surf = 
  do 
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures
    lerpRows_2 vc world lights slices tps surf rl 
               
  where                 
    rl =  [mkRealLine (newFloorCastPoint vc world view x1 y)                 
                      (newFloorCastPoint vc world view x2 y) 
           | y <- [0..(viewportCenterY vc - 1)]]
                    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
 
lerpRows_2 :: ViewConfig 
             -> MapType 
             -> Lights
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [RealLine] 
             -> IO ()     
lerpRows_2 vc world lights slices tps surf rl = 
         lerpRowsC_  vc -- (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) 
                  surf
                  (lightsPtr lights)
                  (lightsNum lights) -- (fromIntegral (length lights))
                  rl



newFloorCastPoint :: ViewConfig -> MapType -> View -> Int32 -> Int32 -> (Float,Float)    
newFloorCastPoint vc world (pos,angle) x y = 
  ps
  where 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (x {-column-} - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    ps = ((point2DGetX pos) - distance * sin radians
         ,(point2DGetY pos) + distance * cos radians)
    
    distance = rowDistance y 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral row 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle

 
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
                                        
  -- These textures are not in the repo yet.          
  wallTextures <- sequence [conv pf =<< loadBMP "../../Data/Wall3.bmp"
                           ,conv pf =<< loadBMP "../../Data/Wall2.bmp"
                           ,conv pf =<< loadBMP "../../Data/Door.bmp"
                           ,conv pf =<< loadBMP "../../Data/DoorOpen.bmp"]
   
  floorTextures <- sequence [conv pf =<< loadBMP "../../Data/Floor1.bmp"
                            ,conv pf =<< loadBMP "../../Data/Floor2.bmp"
                            ,conv pf =<< loadBMP "../../Data/Floor3.bmp"
                            ,conv pf =<< loadBMP "../../Data/Brunn.bmp"]
  
  
  monster <- conv pf =<< loadBMP "../../Data/Eye1.bmp"  
  let monsterSprite = Sprite (mkPoint (5*256+128,1*256+128))
                              0
                              (256,256) 
                              monster
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite (mkPoint (4096,4096))
    (False,False,False,False) -- Keyboard state
    (North,mkPoint (255+127,255+127))
    0
    
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
             -> (ViewDirection,Point2D) 
             -> Int32
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster targ (up,down,left,right) (dir,pos) ly = do 
  
  let (x,y) = (point2DGetX pos, point2DGetY pos)
      lights = ([mkLight (x,y) (1.0,1.0,1.0)] ++ 
                [mkLight ((i+5)*256+128,(j+1)*256+128+(fromIntegral ly)) (1.0,0.0,0.0) 
                | i <- [0,9], j <- [0]])
               
  withLights lights $ \lights' ->              
    do 
       sl <- renderWalls vc
                         testLevelArr 
                         lights' 
                         (pos,direction dir) 
                         wallTextures 
                         screen
                         
       let dists  = map sliceDistance sl
           bots   = map sliceBot      sl 
       newFloorCast3 vc testLevelFloorArr lights' bots (pos,direction dir) floorTextures screen                                     

       
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
  
  -- (r',x',y') <- (moveLeft left' >=> moveRight right' >=> moveUp up' >=> moveDown down') (r,x,y) 
  dir' <-
    if left' 
    then moveLeft dir 
    else if right' 
         then moveRight dir
         else return dir
  
  pos' <- if up'   
          then moveForward dir pos
          else return pos
  
  putStrLn$ show dir'
  -- let pos' = mkPoint (x',y')
      
  unless b $ eventLoop vc screen floorTextures wallTextures monster' targ' (up',down',left',right') (dir',pos') ((ly + 128) `mod` 4096)    
  
  
  -- very crude colision against walls added
  where 
 --    a = 3
    moveRight :: Monad m =>  ViewDirection -> m ViewDirection
    moveRight North = return East 
    moveRight East  = return South
    moveRight South = return West 
    moveRight West  = return North 
    
    moveLeft :: Monad m => ViewDirection -> m ViewDirection 
    moveLeft North = return West 
    moveLeft West  = return South 
    moveLeft South = return East 
    moveLeft East  = return North 
    
    -- TODO: Use grid coords instead of "fine"-coords. 
    moveForward :: Monad m => ViewDirection -> Point2D -> m Point2D 
    moveForward North (Point2D x y) = return$ mkPoint (x,y+256) 
    moveForward East  (Point2D x y) = return$ mkPoint (x+256,y)
    moveForward South (Point2D x y) = return$ mkPoint (x,y-256) 
    moveForward West  (Point2D x y) = return$ mkPoint (x-256,y) 
   
    
     