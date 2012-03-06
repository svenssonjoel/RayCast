{-2012 Joel Svensson -}

module Engine.PortalWorld.RayCast where

import Engine.Math
import Engine.Light 
import Engine.RGB
import Engine.Slice 
import Engine.ViewConfig
import Engine.World

import Engine.PortalWorld.Map

import Data.List hiding (intersect)
import Data.Int

import MathExtras
import CExtras
----------------------------------------------------------------------------
--
maxVisible = 4096
lightRadius = 256 -- remove
textureWidth = 256

---------------------------------------------------------------------------- 

castRay vc world lights (pos,angle) column = 
    do 
      ((),inR,inG,inB) <- computeLight (floori_ px) (floori_ py) (lightsPtr lights) (lightsNum lights)
      return$ Slice top bot texValue texCol (mkRGB inR inG inB) dist  
      
    where 
      ray = mkRayLen pos (angle - colAngle) maxVisible
      col = column - viewportCenterX vc
      colAngle = atan $ fromIntegral col / fromIntegral (vcViewDistance vc)
    
      top  = bot - height 
      bot  = floori_ $ fromIntegral (viewportCenterY vc) + (fromIntegral height / 2) 
      height = floori_ $ fromIntegral (vcViewDistance vc * wallHeight vc) / dist
    
      dist = dist' * cos colAngle
    
      (dist', texValue, texCol, (Point2D px py)) = castRay2 world ray 
 
-- intersect against a wall or portal
wallIntersect :: Ray -> Wall -> Maybe Point2D    
wallIntersect ray (Wall line id)  = intersectSeg ray line   
wallIntersect ray (Portal line v _) = if (vecDot (rayDeltas ray) v <= 0.0) 
                                      then intersectSeg ray line
                                      else Nothing   
  
-- Helper for castRay  
castRay2 :: MapType ->  Ray  -> (Float,Int32,Int32,Point2D)
castRay2 world ray = 
         case wall of 
            (Just (Wall _ _)) -> (d,1,offset,point) -- cheat a bit for now  
            (Just (Portal _ _ w')) -> {-trace "Portal" $-}  castRay2 w' ray 
            Nothing -> (d,1,offset,point)
  where 
    walls = mapWalls world              
    
    -- test intersection against all walls 
    intersections = map (wallIntersect ray) walls
    
    distances     = [(distance (rayStart ray) p,p,Just l) | (Just p,l) <- zip intersections walls]
    dist'         = sortBy (\(x,_,_) (y,_,_) -> compare x y) distances  
    dist          = if null dist' 
                    then (maxVisible,(Point2D 0 0),Nothing) 
                    else  (head dist')
    (_,point,_)         = dist
    
    -- TODO: Clean this mess up 
                                                                   
    d = (\(x,_,_) -> x) dist 
    offset = tex dist 
    wall = (\(_,_,w) -> w) dist      
                    
    tex (_,p,Just (Wall l _)) = floori_ (distanceAlongLine p l)  `mod` textureWidth
    tex (_,_,Just (Portal _ _ _) ) = 45
    tex (_,_,Nothing) = 32         
                       
    minimum' [(a,i)] = (a,i) 
    minimum' ((a1,i1):(a2,i2):xs) = if a1 < a2 
                                    then minimum' ((a1,i1):xs)
                                    else minimum' ((a2,i2):xs)