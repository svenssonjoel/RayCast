{- 
  2012 Joel Svensson   


  RayCasting (Like in the old Wolfenstein 3d game)  

  This is an attempt to Haskellify the code from 
  Christopher Lamptons Book "Gardens of Imagination"  

  **************************************
  This is also an exercise in using SDL. 
  
  Early problems in using SDL is. 
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

import Engine.RayCast
import Engine.Math
import Engine.Map
import Engine.Render
import Engine.RItem
import Engine.Sprite 
import Engine.ZBuffer

import Control.Monad
import Data.Array
import Data.Maybe

import Data.Word
import Data.Bits
import Data.Int

import SDLUtils 

import Foreign.Ptr
import Foreign.Storable 
import Foreign.Marshal.Array

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
             [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --7
             [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --8
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --9
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --10
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --11
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --12
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --13
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], --14
             [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]  --15
            ]

testLevelArr :: Array2D Int32 Int32 
testLevelArr = listArray (0,15) (map (listArray (0,15)) testLevel)
testLevelFloorArr :: Array2D Int32 Int32 
testLevelFloorArr = listArray (0,15) (map (listArray (0,15)) testFloor)


----------------------------------------------------------------------------
-- Constants 

walkSpeed      = 32 

    
----------------------------------------------------------------------------      
-- Cast for floors 
-- The slices are just there to be able to make some optimisations.              
newFloorCast :: ViewConfig -> MapType -> [Light] -> Point2D -> Angle -> [Slice] -> [Surface] -> Surface -> IO ()              
newFloorCast vc world lights pos angle slices textures surf =              
  zipWithM_ (newFloorCastColumn vc world lights pos angle textures surf) slices [0..vcWindowWidth vc -1]
             
newFloorCastColumn :: ViewConfig -> MapType -> [Light] -> Point2D -> Float -> [Surface] -> Surface -> Slice -> Int32 -> IO ()
newFloorCastColumn vc world lights (px,py) angle tex surf slice col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- mapM ((return . castPtr) <=< surfaceGetPixels) tex

    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    -- compute light 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    -- only check floor where there are no walls (optimisation) 
    rows = [sliceBot slice..vcWindowHeight vc-1]
    -- rows = [viewportCenterY+1..windowHeight-1]              


    ps = [(fromIntegral px - distance * sin radians 
          ,fromIntegral py + distance * cos radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral (row - viewportCenterY vc) 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle
         
    renderPoint :: [Ptr Word32] -> Ptr Word32 -> Int32 -> Int32 -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        let (tx,ty) = (floori_ x `div` wallWidth vc, floori_ y `div` wallWidth vc) 
        
        p  <- peekElemOff (tex P.!! (fromIntegral (world !! (tx,ty)))) (fromIntegral t) 
        
        let (inR,inG,inB) = clamp 1.0 $ foldl vec3add (0,0,0) (map (lightContribution (floori_ x,
                                                                                       floori_ y))  lights)
   
        -- let i = (min 1.0 (32768/(dist*dist))) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ inB * (fromIntegral p0) 
            p1' =  floor_ $ inG * (fromIntegral p1) 
            p2' =  floor_ $ inR * (fromIntegral p2) 
                                
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf (fromIntegral r)  p'     -- floor... 
        pokeElemOff surf (fromIntegral r2) p'     -- ceiling...   
        
        where 
          t  = ((floori_ y .&. modMask vc) * textureWidth + (floori_ x .&. modMask vc))
          r  = (row * (vcWindowWidth vc) + col)
          r2 = ((vcWindowHeight vc -row-1) * (vcWindowWidth vc) + col )
          textureWidth = 256 -- fix this !!! surfaceGetWidth tex
  

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
  
  putStrLn$ arr2dStr$ testLevelArr
  
  let pf = surfaceGetPixelFormat screen
                       
  --wallTextures <- sequence [conv pf =<< loadBMP "Data/textureLarge1.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge2.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge1.bmp"
  --                         ,conv pf =<< loadBMP "Data/textureLarge1.bmp"]
                 
  -- These textures are not in the repo yet.          
  wallTextures <- sequence [conv pf =<< loadBMP "Data/Wall3.bmp"
                           ,conv pf =<< loadBMP "Data/Wall2.bmp"
                           ,conv pf =<< loadBMP "Data/Door.bmp"
                           ,conv pf =<< loadBMP "Data/DoorOpen.bmp"]
   
  floorTextures <- sequence [conv pf =<< loadBMP "Data/Floor1.bmp"
                            ,conv pf =<< loadBMP "Data/Floor2.bmp"
                            ,conv pf =<< loadBMP "Data/Floor3.bmp"]
  
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

  
  monster <- conv pf =<< loadBMP "Data/Eye1.bmp"  
  --monster <- conv pf =<< loadBMP "Data/eye1.bmp"  
  let monsterSprite = [Sprite ((x+5)*256+128,(y+1)*256+128)
                              0
                              (256,256) 
                              monster | x <- [0..1], y <- [0]] 
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite
    (False,False,False,False) -- Keyboard state
    (0.0,256+128 ,256+128)
  
  quit
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: ViewConfig 
             -> Surface 
             -> [Surface] 
             -> [Surface] 
             -> [Sprite]
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int32, Int32) 
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
  
  -- draw all the visible walls
  -- floorCast  testLevelFloorArr x y r screen floorTextures
  
  
  let lights = ([Light (x,y) (0.5,0.5,0.5)] ++ 
                         [Light ((i+5)*256+128,(j+1)*256+128) (0.02,0.0,0.0) 
                         | i <- [0..1], j <- [0]])
  slices <- renderWalls vc 
                        testLevelArr 
                        lights 
                        ((x,y),r) 
                        wallTextures 
                        screen
  newFloorCast vc testLevelFloorArr lights (x,y) r slices floorTextures screen
  
  let dists  = map sliceDistance slices 
  
  let monsterTfrmd = sortRItems $ 
                     catMaybes $ 
                     map (viewTransformSprite vc lights ((x,y),r)) monster    
  
  -- sort monsters or update the zbuffer. (probably stick to sort) 
      
  withZBuffer dists (\zbuf -> 
                      sequence_ $ map (renderRItem screen zbuf) monsterTfrmd
                    )
  
  SDL.flip screen
  
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
  
  let (r',x',y') = (moveLeft left' . moveRight right' . moveUp up' . moveDown down') (r,x,y) 

  unless b $ eventLoop vc screen floorTextures wallTextures monster (up',down',left',right') (r',x',y')     
  
  
  -- very crude colision against walls added
  where 
    moveLeft  b (r,x,y) = if b then (r+0.04,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r-0.04,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y + (floori_ ((fromIntegral walkSpeed)*cos r))
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y - (floori_ ((fromIntegral walkSpeed)*cos r))
    movementAllowed (px,py) = testLevelArr !! (px `div` wallWidth vc,py `div` wallWidth vc) == 0
