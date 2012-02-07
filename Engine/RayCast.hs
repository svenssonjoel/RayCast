{-# LANGUAGE ScopedTypeVariables, 
             FlexibleContexts
            #-}
module Engine.RayCast where 


import Data.Int
import Data.Bits 

import Engine.Math
import Engine.Map

import MathExtras 

import Foreign.Storable
import Foreign.Ptr

import Prelude hiding ((!!)) 
import Data.Array.MArray
import Data.Array.Storable

import CExtras

----------------------------------------------------------------------------
-- ViewConfiguration

data ViewConfig = 
  ViewConfig { vcViewDistance :: Int32,
               vcViewHeight   :: Int32,
               vcWindowWidth  :: Int32, 
               vcWindowHeight :: Int32, 
               vcWallDims     :: (Int32,Int32) } 
  
mkViewConfig = ViewConfig 

viewportCenterX = (`div` 2) . vcWindowWidth 
viewportCenterY = (`div` 2) . vcWindowHeight
                  
wallWidth  = fst . vcWallDims 
wallHeight = snd . vcWallDims 

gridMask = negate . modMask  
modMask  vc = wallWidth vc - 1  

----------------------------------------------------------------------------
-- Slices 

data Slice = Slice {sliceTop :: Int32,
                    sliceBot :: Int32, 
                    sliceTex :: Int32,
                    sliceTexCol :: Int32,
                    sliceIntensityR :: Float,  --Group these up 
                    sliceIntensityG :: Float, 
                    sliceIntensityB :: Float,
                    sliceDistance :: Float}

---------------------------------------------------------------------------- 
-- View 

type View = (Point2D, Angle) 



----------------------------------------------------------------------------
-- raycasting  

castRay :: (MArray StorableArray Int32 m, Monad m)
           => ViewConfig 
           -> MapType -- Array2D Int32 Int32 
           -> [Light] 
           -> View 
           -> Int32 
           -> m Slice 
castRay vc world lights (pos,angle) column = 
  do 
  
  let ray  = mkRay pos (angle - columnAngle)
      columnAngle = atan $ fromIntegral col / fromIntegral (vcViewDistance vc)
      col = column - viewportCenterX vc
  
      
      
  (dist', texValue, texCol,(inR,inG,inB)) <- castRay2 vc world lights 0.0 ray   
  let dist = dist' * cos columnAngle
      height = floori_ $ fromIntegral (vcViewDistance vc * wallHeight vc) / dist
      top  = bot - height 
      bot  = floori_ $ fromIntegral (viewportCenterY vc) + (fromIntegral height / 2) 

  return $ Slice top 
                 bot 
                 texValue 
                 texCol 
                 -- (min 1.0 (32768 {-lightradius-}/(dist*dist))) 
                 inR inG inB 
                 dist 
  where 
    
--    (dist', texValue, texCol,(inR,inG,inB)) = castRay2 vc world lights 0.0 ray 
    
   
lightContribution (px,py) (Light  lx ly   inR' inG' inB' ) = (inR,inG,inB)    
  where
    -- How to really compute light contribution? 
    lightdist = (distance (px,py) (lx,ly) / 256) 
    ld = max 0.01 (lightdist * lightdist)
    (inR,inG,inB) = (min 1.0 (inR'/ld),
                     min 1.0 (inG'/ld),
                     min 1.0 (inB'/ld))
                    
vec3add (x,y,z) (u,v,w) = (x+u,y+v,z+w)
clamp i (x,y,z) = (min x i, min y i, min z i) 

----------------------------------------------------------------------------
-- castRay2
castRay2 :: (MArray StorableArray Int32 m, Monad m) 
            => ViewConfig 
            -> MapType 
            -> [Light] 
            -> Float 
            -> Ray  
            -> m (Float,Int32,Int32,(Float,Float,Float))
castRay2 vc world lights accDist ray = 
    do 
      value <- world !! (px `div` wallWidth vc, py `div` wallWidth vc)
      if (value > 0) -- ray has struck solid wall
        then 
          return (accDist+dist,value,offs,(inR,inG,inB)) 
        else 
          -- Continue along the ray 
          castRay2 vc world lights (accDist+dist) (Ray (px ,py) (rayDeltas ray))

        
  where 
    grid_x = if (posRayDx ray) 
             then (rayX ray .&. gridMask vc) + wallWidth vc
             else (rayX ray .&. gridMask vc) - 1
    grid_y = if (posRayDy ray) 
             then (rayY ray .&. gridMask vc) + wallWidth vc
             else (rayY ray .&. gridMask vc) -1 
                  
    -- Experimental lighting 
    (inR,inG,inB) = clamp 1.0 $ foldl vec3add (0,0,0) (map (lightContribution (px,py))  lights)
  
    
    
    -- Create two lines for intersection test
    x_line = Line (fromIntegral grid_x,0) (fromIntegral grid_x,1) 
    y_line = Line (0,fromIntegral grid_y) (1,fromIntegral grid_y)  
    
    -- intersect ray with both vertical and horizontal line
    -- the closest one is used. 
    x_intersect = intersect ray x_line 
    y_intersect = intersect ray y_line
    
    
    
    
    ((px,py),dist,offs)  = 
      case (x_intersect,y_intersect) of 
        (Nothing,Nothing) -> error "Totally impossible" 
        (Just p, Nothing) -> (p, distance (rayStart ray) p,(snd p `mod` wallWidth vc) )
        (Nothing, Just p) -> (p, distance (rayStart ray) p,(fst p `mod` wallWidth vc) ) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 < d2 
             then (p,d1,(snd p `mod` wallWidth vc) ) 
             else (q,d2,(fst q `mod` wallWidth vc) ) 

