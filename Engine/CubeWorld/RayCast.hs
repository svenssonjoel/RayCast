{-# LANGUAGE ScopedTypeVariables, 
             FlexibleContexts
            #-}
{- 2012 Joel Svensson -}

module Engine.CubeWorld.RayCast where 


import Data.Int
import Data.Bits 

import Engine.Math
import Engine.Light 
import Engine.RGB
import Engine.Slice 
import Engine.ViewConfig
import Engine.World

import Engine.CubeWorld.Map


import MathExtras 

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Prelude hiding ((!!)) 
import Data.Array.MArray
import Data.Array.Storable
import Data.Maybe
import Data.List hiding (intersect,(!!))

import CExtras

import System.IO.Unsafe

----------------------------------------------------------------------------
-- raycasting  in "Cube"-World

floorPoint (Point2D x y) = 
  Point2D (fromIntegral (floor x)) 
          (fromIntegral (floor y))
floorVector (Vector2D x y) = 
  Vector2D (fromIntegral (floor x)) 
           (fromIntegral (floor y))

castRay :: ViewConfig 
           -> MapType 
           -> Lights
           -> View 
           -> Int32 
           -> IO Slice 
castRay vc world lights (pos,angle) column = 
  do 
  
  let --ray = mkRay pos (angle - columnAngle)
      -- NOTE: forcing ray endpoint to integer locations. 
      --       
      ray = Ray (floorPoint pos) 
                (floorVector (mkVector (-8192*sin (angle - columnAngle), 
                                         8192*cos (angle - columnAngle))))
            
     -- ray = mkRayLen pos 
     --               (angle - columnAngle)
     --               8192

      columnAngle = atan $ fromIntegral col / fromIntegral (vcViewDistance vc)
      col = column - viewportCenterX vc
  
      
      
  --(dist', texValue, texCol,(inR,inG,inB)) <- castRay2 vc world lights 0.0 ray   
  (dist', texValue, texCol,(inR,inG,inB)) <- castRay3 vc world lights ray     
  let dist = dist' * cos columnAngle
      height = floor $ fromIntegral (vcViewDistance vc * wallHeight vc) / dist
      top  = bot - height 
      bot  = floor $ fromIntegral (viewportCenterY vc) + (fromIntegral height / 2) 

  return $ Slice top 
                 bot 
                 texValue 
                 texCol 
                 (mkRGB inR inG inB)
                 dist 
  where 
    
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
          castRay2 vc world lights (accDist+dist) (Ray (Point2D (fromIntegral px) (fromIntegral py)) {-posIntersect-} (rayDeltas ray))

        
  where 
    grid_x = if (posRayDx ray) 
             then (rayXi ray .&. gridMask vc) + wallWidth vc
             else (rayXi ray .&. gridMask vc) - 1
    grid_y = if (posRayDy ray) 
             then (rayYi ray .&. gridMask vc) + wallWidth vc
             else (rayYi ray .&. gridMask vc) - 1
                  
    
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
        (Just p, Nothing) -> (p, distance (rayStart ray) p, (point2DGetYi p .&. modMask vc)) -- `mod` wallWidth vc) )
        (Nothing, Just p) -> (p, distance (rayStart ray) p, (point2DGetXi p .&. modMask vc)) -- <`mod` wallWidth vc) ) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 <= d2 
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
   





----------------------------------------------------------------------------
-- castRay3
castRay3 :: ViewConfig 
            -> MapType 
            -> Lights
            -> Ray  
            -> IO (Float,Int32,Int32,(Float,Float,Float))
castRay3 vc world lights ray = 
    do 
      
      (dist,value,offs,(Point2D px py) ) <- intersectGrid ray world (mapGrid world 256) 
      ((),inR,inG,inB) <- computeLight (floor px) (floor py) (lightsPtr lights) (lightsNum lights)
      return (dist,value,offs,(inR,inG,inB))

  where 
    intersectGridLine:: Ray -> MapType -> Line -> IO (Maybe (Int32,Line,Point2D))
    intersectGridLine r m l = 
      case posIntersect of 
        Nothing -> return Nothing 
        (Just pos@(Point2D px py)) -> 
          -- TODO: Figure out why this works ! 
          let px' = if (posRayDx ray) 
                    then (floor px) `div` 256  
                    else (floor px - 1) `div` 256
              py' = if (posRayDy ray) 
                    then (floor py) `div` 256 
                    else (floor py - 1) `div` 256 
          in 
           do 
             -- putStrLn$ show (px',py')
             if ( px' >= 0 && px' < mapWidth m && 
                  py' >= 0 && py' < mapHeight m) 
               then 
                do 
                 v <- m !! (px',py') 
                 if v > 0 
                   then return $ Just (v,l,pos) 
                   else return Nothing  
               else return Nothing 
      where 
        posIntersect = intersectSeg r l

    intersectGrid :: Ray -> MapType -> [Line] -> IO (Float,Int32,Int32,Point2D)
    intersectGrid r m ls = 
      do 
        intersections <- mapM (intersectGridLine r m) ls
        
        let intersections' = catMaybes intersections
        
            sorted = sortBy (\(_,_,p) (_,_,q) -> compare (distance (rayStart ray) p)
                                                         (distance (rayStart ray) q)) intersections' 
            
            closest@(dist,v,offs,posIntersect) = 
                        if (null sorted) 
                        then (8192,1,1,Point2D 0 0)
                        else 
                          let (v,l,pI) = head sorted 
                          in (distance (rayStart ray) pI,
                              v,
                              floor (distanceAlongLine pI l) `mod` 256,
                              pI)
                              
        return (dist,v,offs,posIntersect)
        
    