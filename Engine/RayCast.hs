module Engine.RayCast where 


import Data.Int
import Data.Bits 

import Engine.Math
import Engine.Map

import MathExtras 

import Prelude hiding ((!!)) 

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
                    sliceIntensity :: Float,
                    sliceDistance :: Float}

---------------------------------------------------------------------------- 
-- View 

type View = (Point2D, Angle) 


----------------------------------------------------------------------------
-- raycasting  

castRay :: ViewConfig -> Array2D Int32 Int32 -> View -> Int32 -> Slice 
castRay vc world (pos,angle) column = 
  Slice top 
        bot 
        texValue 
        texCol 
        (min 1.0 (32768 {-lightradius-}/(dist*dist))) 
        dist 
  where 
    ray  = mkRay pos (angle - columnAngle)
    columnAngle = atan $ fromIntegral col / fromIntegral (vcViewDistance vc)
    top  = bot - height 
    bot  = floori_ $ fromIntegral (viewportCenterY vc) + (fromIntegral height / 2) 
    height = floori_ $ fromIntegral (vcViewDistance vc * wallHeight vc) / dist
    dist = dist' * cos columnAngle
    col = column - viewportCenterX vc
    (dist', texValue, texCol) = castRay2 vc world 0.0 ray 
 
   
    


----------------------------------------------------------------------------
-- castRay2
castRay2 :: ViewConfig -> Array2D Int32 Int32 -> Float -> Ray  -> (Float,Int32,Int32)
castRay2 vc world accDist ray = 
    if (value > 0) -- ray has struck solid wall
    then (accDist+dist,value,offs) 
    else 
      -- Continue along the ray 
      castRay2 vc world (accDist+dist) (Ray (px ,py) (rayDeltas ray))

        
  where 
    grid_x = if (posRayDx ray) 
             then (rayX ray .&. gridMask vc) + wallWidth vc
             else (rayX ray .&. gridMask vc) - 1
    grid_y = if (posRayDy ray) 
             then (rayY ray .&. gridMask vc) + wallWidth vc
             else (rayY ray .&. gridMask vc) -1 
                  
    
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
    value = world !! (px `div` wallWidth vc, py `div` wallWidth vc)
