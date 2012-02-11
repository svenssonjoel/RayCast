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

import Engine.RayCast
import Engine.Math
import Engine.Map
import Engine.Render
import Engine.RItem
import Engine.Sprite 
import Engine.ZBuffer
import Engine.Light 

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

    
----------------------------------------------------------------------------      
-- Cast for floors 
newFloorCast :: ViewConfig 
                -> MapType 
                -> [Light]
                -> [Int32]
                -> View 
                -> [Surface] 
                -> Surface 
                -> IO ()
newFloorCast vc world lights slices view textures surf = 
  do 
    
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures

    mapM_ (lerpRow_ vc world lights slices tps surf)  edges
               
  where 
    edges = [
             (y+viewportCenterY vc,(newFloorCastPoint vc world view x1 y,
                 newFloorCastPoint vc world view x2 y))
            | y <- [0..(viewportCenterY vc-1)]] 
    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
    
lerpRow_ :: ViewConfig -> MapType -> [Light] -> [Int32] -> [(Pixels,Int32)] -> Surface -> (Int32, ((Float,Float),(Float,Float))) -> IO ()     
lerpRow_ vc world lights slices tps surf (y,(p1,p2)) = 
         lerpRowC (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) -- (castPtr tps') 
                  surf
                  lights                  
                  (fromIntegral (length lights))
                  y
                  (fst p1) (snd p1)
                  (fst p2) (snd p2)
             
newFloorCast2 :: ViewConfig 
                 -> MapType 
                 -> [Light]
                 -> [Int32]
                 -> View 
                 -> [Surface] 
                 -> Surface 
                 -> IO ()
newFloorCast2 vc world lights slices view textures surf = 
  do 
    
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures
    lerpRows_ vc world lights slices tps surf x1s y1s x2s y2s 
               
  where 
    -- TODO: Storable points. 
    -- TODO: Storable Pairs of points. (lines)  
    (x1s,y1s) = unzip edgeL
    (x2s,y2s) = unzip edgeR
    
    edgeL = [newFloorCastPoint vc world view x1 y
            | y <- [0..(viewportCenterY vc-1)]] 
    edgeR = [newFloorCastPoint vc world view x2 y
            | y <- [0..(viewportCenterY vc-1)]] 
    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
    

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
    lerpRows_2 vc world lights slices tps surf x1s y1s x2s y2s 
               
  where 
    -- TODO: Storable points. 
    -- TODO: Storable Pairs of points. (lines)  
    (x1s,y1s) = unzip edgeL
    (x2s,y2s) = unzip edgeR
    
    edgeL = [newFloorCastPoint vc world view x1 y
            | y <- [0..(viewportCenterY vc-1)]] 
    edgeR = [newFloorCastPoint vc world view x2 y
            | y <- [0..(viewportCenterY vc-1)]] 
    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 



lerpRows_ :: ViewConfig 
             -> MapType 
             -> [Light] 
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> IO ()     
lerpRows_ vc world lights slices tps surf p1x p1y p2x p2y = 
         lerpRowsC (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) -- (castPtr tps') 
                  surf
                  lights                  
                  (fromIntegral (length lights))
                  p1x p1y
                  p2x p2y


lerpRows_2 :: ViewConfig 
             -> MapType 
             -> Lights
           --  -> Ptr Light 
           --  -> Int32
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> IO ()     
lerpRows_2 vc world lights slices tps surf p1x p1y p2x p2y = 
         lerpRowsC_ (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) -- (castPtr tps') 
                  surf
                  (lightsPtr lights)
                  (lightsNum lights) -- (fromIntegral (length lights))
                  p1x p1y
                  p2x p2y




newFloorCastPoint :: ViewConfig -> MapType -> View -> Int32 -> Int32 -> (Float,Float)    
newFloorCastPoint vc world (pos,angle) x y = 
  ps
  where 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (x {-column-} - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    ps = (fromIntegral (fst pos) - distance * sin radians
         ,fromIntegral (snd pos) + distance * cos radians)
    
    distance = rowDistance y 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral row 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle


{-     
lerpRow :: ViewConfig -> MapType -> [Light] -> Array Int Int32 -> Array Int (Ptr CInt,Int32) -> Surface -> (Int32, ((Float,Float),(Float,Float))) -> IO () 
lerpRow vc world lights slices tps surf (y,(p1,p2)) = 
  do 
    sp <- castPtr `fmap` surfaceGetPixels surf
    sequence_ [do 
                  let (inx,iny) = ((floori_ (fromIntegral xi * rX+(fst p1))),
                                   (floori_ (fromIntegral xi * rY+(snd p1)))) 
                      (inR,inG,inB) = clamp 1.0 $ foldl1 vec3add (map (lightContribution (inx,iny))  lights)
                      (wx,wy) = ((inx `div` wallWidth vc) .&. 15, 
                                 (iny `div` wallWidth vc) .&. 15) 
                      (tx,ty) = (inx .&. modMask vc,
                                 iny .&. modMask vc)
                      
                  -- texture mapping    
                  tix  <- fmap fromIntegral (world !! (wx,wy))
                  let (tp,w) = (tps  ! tix ) 
                      t      = tx + ty * (fromIntegral w) -- fromIntegral (surfaceGetWidth tex)   
                 
                  
                
                  if (slices ! (fromIntegral xi) <= y)     
                     -- speeds up quite a bit when not much floor is visible
                    then 
                     do 
                      texPointC tx ty w tp xi y width sp inR inG inB
                      texPointC tx ty w tp xi (vcWindowHeight vc - y) width sp inR inG inB
                    else 
                      return () 
                  {- 
                  (p :: Word32) <- peekElemOff tp (fromIntegral t)  
                  let p0  = p .&. 255 
                      p1  = p `shiftR` 8 .&. 255 
                      p2  = p `shiftR` 16 .&. 255 
                      -- p3  = p `shiftR` 24 .&. 255 
                      p0' =  floor_ $ inB * (fromIntegral p0) 
                      p1' =  floor_ $ inG * (fromIntegral p1) 
                      p2' =  floor_ $ inR * (fromIntegral p2) 
                                
                      p'  = p0' -- + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
                  pokeElemOff sp (fromIntegral (xi+y*width)) p'     -- floor... 
                  -}
              | xi <- [0..width-1]]
    
  where 
    
    rX = (fst p2 - fst p1) / fromIntegral width
    rY = (snd p2 - snd p1) / fromIntegral width
    width = vcWindowWidth vc
    x1 = 0; 
    

   -} 



-- The slices are just there to be able to make some optimisations.              
{-     
floorCast :: ViewConfig -> MapType -> [Light] -> Point2D -> Angle -> [Slice] -> [Surface] -> Surface -> IO ()              
floorCast vc world lights pos angle slices textures surf =              
  zipWithM_ (floorCastColumn vc world lights pos angle textures surf) slices [0..vcWindowWidth vc -1]
             
floorCastColumn :: ViewConfig -> MapType -> [Light] -> Point2D -> Float -> [Surface] -> Surface -> Slice -> Int32 -> IO ()
floorCastColumn vc world lights (px,py) angle tex surf slice col = 
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
        
        index <- fmap fromIntegral (world !! (tx,ty))
        p  <- peekElemOff (tex P.!! index)  (fromIntegral t) 
        
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
  
-} 
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
  wallTextures <- sequence [conv pf =<< loadBMP "Data/Wall3.bmp"
                           ,conv pf =<< loadBMP "Data/Wall2.bmp"
                           ,conv pf =<< loadBMP "Data/Door.bmp"
                           ,conv pf =<< loadBMP "Data/DoorOpen.bmp"]
   
  floorTextures <- sequence [conv pf =<< loadBMP "Data/Floor1.bmp"
                            ,conv pf =<< loadBMP "Data/Floor2.bmp"
                            ,conv pf =<< loadBMP "Data/Floor3.bmp"
                            ,conv pf =<< loadBMP "Data/Brunn.bmp"]
  
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
  let monsterSprite = [Sprite ((x+5)*256+128,(y+1)*256+128)
                              0
                              (256,256) 
                              monster | x <- [0..10], y <- [0..10]] 
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite
    (False,False,False,False) -- Keyboard state
    (0.0,256+128 ,256+128)
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
             -> [Sprite]
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int32, Int32) 
             -> Int32
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster (up,down,left,right) (r,x,y) ly = do 
  
  let lights = ([mkLight (x,y) (1.0,1.0,1.0)] ++ 
                [mkLight ((i+5)*256+128,(j+1)*256+128+ly) (0.0,1.0,0.0) 
                | i <- [0], j <- [0]])
               
  withLights lights $ \lights' ->              
    do 
       sl <- renderWalls vc
                         testLevelArr 
                         lights' 
                         ((x,y),r) 
                         wallTextures 
                         screen
                         
       let dists  = map sliceDistance sl
           bots   = map sliceBot      sl 
       newFloorCast3 vc testLevelFloorArr lights' bots ((x,y),r) floorTextures screen                                     

       
       let monsterTfrmd = sortRItems $ 
                          catMaybes $ 
                          map (viewTransformSprite vc ((x,y),r)) monster    
       
       withZBuffer dists $ \zbuf -> 
         sequence_ $ map (renderRItem screen zbuf lights') monsterTfrmd
                    
                             
       
       
    
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
  
  (r',x',y') <- (moveLeft left' >=> moveRight right' >=> moveUp up' >=> moveDown down') (r,x,y) 

  unless b $ eventLoop vc screen floorTextures wallTextures monster (up',down',left',right') (r',x',y') ((ly + 128) `mod` 4096)    
  
  
  -- very crude colision against walls added
  where 
    a = 3
    moveLeft :: Monad m => Bool -> (Float,Int32,Int32) -> m (Float,Int32,Int32)
    moveLeft  b (r,x,y) = return $ if b then (r+0.04,x,y) else (r,x,y) 
    moveRight :: Monad m => Bool -> (Float,Int32,Int32) -> m (Float,Int32,Int32)
    moveRight b (r,x,y) = return $ if b then (r-0.04,x,y) else (r,x,y) 
    
    moveUp :: (MArray StorableArray Int32 m,Monad m) => Bool -> (Float,Int32,Int32) -> m (Float,Int32,Int32)
    moveUp    b (r,x,y) = 
      do 
        ma <- movementAllowed (x',y')
        return $ if b && ma then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y + (floori_ ((fromIntegral walkSpeed)*cos r))
    moveDown :: (MArray StorableArray Int32 m,Monad m) => Bool -> (Float,Int32,Int32) -> m (Float,Int32,Int32)
    moveDown  b (r,x,y) = 
      do 
        ma <- movementAllowed (x',y')
        return $ if b && ma  then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y - (floori_ ((fromIntegral walkSpeed)*cos r))
        
    movementAllowed ::(MArray StorableArray Int32 m,  Monad m) => (Int32,Int32) -> m Bool    
    movementAllowed (px,py) = 
      do 
        value <- testLevelArr !! (px `div` wallWidth vc,py `div` wallWidth vc)
        return$ value == 0
     