
module Engine.PortalWorld.Render where 

import Control.Monad
import Data.Int
import Prelude as P 
       
import Graphics.UI.SDL

import Engine.PortalWorld.Map       
import Engine.PortalWorld.RayCast

import Engine.Math
import Engine.Light
import Engine.ViewConfig
import Engine.Slice
import Engine.World 

import CExtras

instance World MapType where 
  renderView vc world lights (pos,angle) textures surf = 
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
        
