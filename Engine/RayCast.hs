{-# LANGUAGE ScopedTypeVariables, 
             FlexibleContexts
            #-}
{- 2012 Joel Svensson -}

module Engine.RayCast where 


import Data.Int
import Data.Bits 

import Engine.Math
import Engine.Map
import Engine.Light 
import Engine.RGB
import Engine.Slice 
import Engine.ViewConfig
import Engine.World


import MathExtras 

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Prelude hiding ((!!)) 
import Data.Array.MArray
import Data.Array.Storable

import CExtras

----------------------------------------------------------------------------
-- raycasting  in "Cube"-World

instance World MapType where 
  castRay = castRay' 


castRay' :: ViewConfig 
           -> MapType 
           -> Lights
           -> View 
           -> Int32 
           -> IO Slice 
castRay' vc world lights (pos,angle) column = 
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
                 inR inG inB 
                 dist 
  where 
    
--    (dist', texValue, texCol,(inR,inG,inB)) = castRay2 vc world lights 0.0 ray 
    
lightContribution :: Point2D -> Light -> (Float,Float,Float)   
lightContribution pos (Light lx ly inR' inG' inB' ) = (inR,inG,inB)    
  where
    -- How to really compute light contribution? 
    lightdist = distance pos (mkPoint (lx,ly))
    ld = 1/(0.02*lightdist)
    (inR,inG,inB) = (inR'*ld,
                     inG'*ld,
                     inB'*ld)
                    
vec3add (x,y,z) (u,v,w) = (x+u,y+v,z+w)
clamp i (x,y,z) = (min x i, min y i, min z i) 

----------------------------------------------------------------------------
-- castRay2
castRay2 :: --(MArray StorableArray Int32 m, Monad m) 
            -- => 
            ViewConfig 
            -> MapType 
            -> Lights
            -> Float 
            -> Ray  
            -> IO (Float,Int32,Int32,(Float,Float,Float))
castRay2 vc world lights accDist ray = 
    do 
      value <- world !! (px `div` wallWidth vc,
                         py `div` wallWidth vc)
      if (value > 0) -- ray has struck solid wall
        then 
          do 
            ((),inR,inG,inB) <- computeLight px py (lightsPtr lights) (lightsNum lights)
            return (accDist+dist,value,offs,(inR,inG,inB)) 
        else 
          -- Continue along the ray 
          castRay2 vc world lights (accDist+dist) (Ray posIntersect{-(px ,py)-} (rayDeltas ray))

        
  where 
    grid_x = if (posRayDx ray) 
             then (rayXi ray .&. gridMask vc) + wallWidth vc
             else (rayXi ray .&. gridMask vc) - 1
    grid_y = if (posRayDy ray) 
             then (rayYi ray .&. gridMask vc) + wallWidth vc
             else (rayYi ray .&. gridMask vc) -1 
                  
    
    -- Create two lines for intersection test
    x_line = Line (mkPoint (fromIntegral grid_x,0)) 
                  (mkPoint (fromIntegral grid_x,1)) 
    y_line = Line (mkPoint (0,fromIntegral grid_y)) 
                  (mkPoint (1,fromIntegral grid_y))  
    
    -- intersect ray with both vertical and horizontal line
    -- the closest one is used. 
    x_intersect = intersect ray x_line 
    y_intersect = intersect ray y_line
    
    
    
    (px,py) = (point2DGetXi posIntersect, 
               point2DGetYi posIntersect)
    (posIntersect,dist,offs)  = 
      case (x_intersect,y_intersect) of 
        (Nothing,Nothing) -> error "Totally impossible" 
        (Just p, Nothing) -> (p, distance (rayStart ray) p,(point2DGetYi p .&. modMask vc)) -- `mod` wallWidth vc) )
        (Nothing, Just p) -> (p, distance (rayStart ray) p,(point2DGetXi p .&. modMask vc)) -- <`mod` wallWidth vc) ) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 < d2 
             then (p,d1,(point2DGetYi p .&. modMask vc)) -- `mod` wallWidth vc) ) 
             else (q,d2,(point2DGetXi q .&. modMask vc)) -- `mod` wallWidth vc) ) 
    point2DGetXi :: Point2D -> Int32
    point2DGetXi = floor . point2DGetX
    point2DGetYi :: Point2D -> Int32
    point2DGetYi = floor . point2DGetY
    rayXi :: Ray -> Int32
    rayXi = floor . rayX
    rayYi :: Ray -> Int32
    rayYi = floor . rayY
    