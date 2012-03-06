
module Engine.CubeWorld.Render where

import Graphics.UI.SDL

import Data.Int
import Control.Monad
import Prelude as P

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
  renderView vc world@(MapType w h dat) lights (pos,angle) textures surf = 
    do 
      slices <- mapM (castRay vc world lights (pos,angle))  [0..vcWindowWidth vc-1]
    
      zipWithM_ (drawSlice textures surf) [0..vcWindowWidth vc-1] slices 
      return slices
   

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
        
    
