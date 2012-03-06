{- 2012 Joel Svensson -} 

module Engine.CubeWorld.Render where

import Graphics.UI.SDL

import Data.Int
import Control.Monad
import Prelude as P
import Data.Array.Storable

import Foreign.Ptr

import Engine.Math       
import Engine.Light
import Engine.Slice
import Engine.ViewConfig
import Engine.World


import Engine.CubeWorld.Map
import Engine.CubeWorld.RayCast


import CExtras

----------------------------------------------------------------------------
-- World Instance 
    

instance World MapType where 
  renderView vc world lights (pos,angle) _ surf = 
    do 
      slices <- mapM (castRay vc world lights (pos,angle))  [0..vcWindowWidth vc-1]
    
      zipWithM_ (drawSlice (mapWallTextures world)  surf) [0..vcWindowWidth vc-1] slices 
      
      -- Now go on and draw the floors  
      let bots  = map sliceBot slices 
          
      floorCast vc world lights bots (pos,angle) (mapFloorTextures world) surf 
      
      -- TODO: Maybe return just a zBuffer? 
      return slices -- in case they are needed elsewhere
   

drawSlice :: [Surface] -> Surface -> Int32 -> Slice -> IO () 
drawSlice textures surf col slice = 
  texVLineLit col
              (sliceTop slice)
              (sliceBot slice)
              surf 
              (sliceTexCol slice)
              0 
              (fromIntegral (textureHeight)) 
              texture
              (sliceIntensity slice)
  where 
    texture = textures  P.!! (fromIntegral (sliceTex slice - 1))
    textureHeight = surfaceGetHeight texture
        
    
---------------------------------------------------------------------------- 
-- Floor drawing 


floorCast :: ViewConfig 
             -> MapType
             -> Lights 
             -> [Int32]
             -> View 
             -> [Surface] 
             -> Surface 
             -> IO ()
floorCast vc world lights slices view textures surf = 
  do 
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures
    lerpRows_2 vc (mapFloor world) (mapWidth world) (mapHeight world) lights slices tps surf rl 
               
  where                 
    rl =  [mkRealLine (floorCastPoint vc view x1 y)                 
                      (floorCastPoint vc view x2 y) 
           | y <- [0..(viewportCenterY vc - 1)]]
                    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
 
lerpRows_2 :: ViewConfig 
             -> (StorableArray Int32 Int32) 
             -> Int32 -> Int32
             -> Lights
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [RealLine] 
             -> IO ()     
lerpRows_2 vc world w h lights slices tps surf rl = 
         lerpRowsC_  vc -- (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  w h world 
                  slices 
                  (map fst tps) 
                  surf
                  (lightsPtr lights)
                  (lightsNum lights) -- (fromIntegral (length lights))
                  rl



floorCastPoint :: ViewConfig -> View -> Int32 -> Int32 -> (Float,Float)    
floorCastPoint vc (pos,angle) x y = 
  ps
  where 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (x {-column-} - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    ps = ((point2DGetX pos) - distance * sin radians
         ,(point2DGetY pos) + distance * cos radians)
    
    distance = rowDistance y 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral row 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle

 