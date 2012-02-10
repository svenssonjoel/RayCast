{- 
   2012 Joel Svensson 
-} 

module Engine.Render where 

import Prelude hiding ((!!))
import qualified Prelude as P

import Graphics.UI.SDL
import Data.Int
import Control.Monad

import Engine.RayCast
import Engine.Math
import Engine.Map 
import Engine.Light

import SDLUtils 
import CExtras
import MathExtras

--remove
import Foreign.Ptr




----------------------------------------------------------------------------
-- rendering routines 
    
-- renderWalls, to replace renderView
renderWalls :: ViewConfig 
               -> MapType {- Array2D Int32 Int32-}  
               -> Lights
               -- -> Ptr Light  -- [Light] 
               -- -> Int
               -> View 
               -> [Surface] 
               -> Surface 
               -> IO [Slice]
renderWalls vc world lights (pos,angle) textures surf = 
  do 
    slices <- mapM (castRay vc world lights (pos,angle))  [0..vcWindowWidth vc-1]
    zipWithM_ (drawSlice textures surf) [0..vcWindowWidth vc-1] slices 
    return slices
   
    
    
drawSlice :: [Surface] -> Surface -> Int32 -> Slice -> IO () 
drawSlice textures surf col slice = 
  texVLineLit (fromIntegral col) 
              (fromIntegral (sliceTop slice)) 
              (fromIntegral (sliceBot slice)) 
              surf 
              (fromIntegral (sliceTexCol slice))
              0 
              (textureHeight) 
              texture
              (sliceIntensityR slice) 
              (sliceIntensityG slice)
              (sliceIntensityB slice) 
  where 
    texture = textures  P.!! (fromIntegral (sliceTex slice - 1))
    textureHeight = surfaceGetHeight texture
        

{-
-- draw a single column into surf
renderCol vc surf textures ((dist,i,x),c) = 
  -- vertLine c starty endy color surf
  -- texturedVLine c starty endy surf  x 0 64 tex
  -- texVLine c starty endy surf x 0 textureHeight tex
  
  texVLineLit (fromIntegral c) 
              (fromIntegral starty) 
              (fromIntegral endy) 
              surf 
              (fromIntegral x) 
              0 
              textureHeight 
              texture
              (min 1.0 (128{-lightRadius-}/dist)) 

  where 
    height = floori_ (fromIntegral (vcViewDistance vc* wallHeight vc) / dist)
    starty = endy - height 
    endy   = floori_ (fromIntegral (viewportCenterY vc) + ((fromIntegral height) / 2))
    textureHeight = surfaceGetHeight texture
    texture = textures P.!! (fromIntegral i - 1) 
-}   

