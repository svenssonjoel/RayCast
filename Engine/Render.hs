{- 
   2012 Joel Svensson 
-} 

module Engine.Render where 

import Prelude hiding ((!!))
import qualified Prelude as P

import Graphics.UI.SDL
import Data.Int
import Control.Monad

import Engine.Math
import Engine.Light
import Engine.Slice
import Engine.World

import SDLUtils 
import CExtras
import MathExtras



----------------------------------------------------------------------------
-- rendering routines 
renderWalls :: World w => ViewConfig 
               -> w
               -> Lights
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
-- Rendering with multisampling 
renderWalls3Samples  :: World w => ViewConfig 
                       -> w
                       -> Lights
                       -> View 
                       -> [Surface] 
                       -> Surface 
                       -> IO [Slice] -- Average Slice! 
renderWalls3Samples vc world lights (pos,angle) textures surf = 
  do     
    -- TODO: try offsetting the samples by just 0.5 (needs to change type to float on column though)
    -- TODO: or implement another "castRay" that returns a multisample Slice of some kind. 
    slices <- mapM (mapM (castRay vc world lights (pos,angle))) [[x-1,x,x+1]| x <- [0..vcWindowWidth vc-1]]
    zipWithM (drawSlice3Samples textures surf) [0..vcWindowWidth vc-1] slices
    -- return (map head slices) -- really compute avg values. 


drawSlice3Samples :: [Surface] -> Surface -> Int32 -> [Slice] -> IO Slice
drawSlice3Samples textures surf col [s1,s2,s3] = 
  do
    texVLineLit3S col
                  avgTop
                  avgBot
                  surf 
                  tc1 tc2 tc3
                  0 
                  (fromIntegral (textureHeight)) 
                  t1 t2 t3
                  (sliceIntensity s2) -- for now!
    return $ Slice avgTop avgBot (-1) (-1) (sliceIntensity s2) avgDist
  where 
    avgTop  = (sliceTop s1 + sliceTop s2 + sliceTop s3) `div` 3 
    avgBot  = (sliceBot s1 + sliceBot s2 + sliceBot s3) `div` 3  
    avgDist = (sliceDistance s1 + sliceDistance s2 + sliceDistance s3) / 3  
    (tc1,tc2,tc3) = (sliceTexCol s1, sliceTexCol s2, sliceTexCol s3)
    (t1,t2,t3)    = (textures P.!! (fromIntegral (sliceTex s1-1)),
                     textures P.!! (fromIntegral (sliceTex s2-1)),
                     textures P.!! (fromIntegral (sliceTex s3-1)))

    textureHeight = surfaceGetHeight t1 --just assume this is ok. 
        