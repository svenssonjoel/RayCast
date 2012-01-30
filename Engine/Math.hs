module Engine.Math where 

import Data.Int

import MathExtras
----------------------------------------------------------------------------
-- Angles, Points, Vectors 
type Angle = Float 
type Point2D = (Int32,Int32) 
type Vector2D = (Int32,Int32) 

vecAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2) 
vecSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

distance :: Point2D -> Point2D -> Float 
distance (x1,y1) (x2,y2) = 
  sqrt $ xd*xd+yd*yd 
  where 
    xd = fromIntegral $ x2 - x1 
    yd = fromIntegral $ y2 - y1
    

----------------------------------------------------------------------------
-- Rays 

data Ray = Ray {rayStart  :: Point2D,
                rayDeltas :: Vector2D}  
           -- point direction repr 

mkRay :: Point2D -> Angle -> Ray 
mkRay p r = Ray p (floori_ (-1024.0*sin r),
                   floori_ ( 1024.0*cos r)) 

posRayDx  r = rayDx r > 0 
posRayDy  r = rayDy r > 0 

rayDx r = fst $ rayDeltas r 
rayDy r = snd $ rayDeltas r 

rayX  r = fst $ rayStart r 
rayY  r = snd $ rayStart r


----------------------------------------------------------------------------
-- Lines 

data Line = Line Point2D Point2D -- 2 points on the line  

mkLine = Line 

----------------------------------------------------------------------------
-- 

intersect :: Ray -> Line -> Maybe Vector2D 
intersect (Ray p1 d1) (Line p2 d2) = if det == 0 
                                     then Nothing 
                                     else (Just (x,y)) 
 where  
   (a1,b1,c1) = convertRay p1 d1
   (a2,b2,c2) = convertLine p2 d2 
   det = a1*b2 - a2*b1
   
   x = (b2*c1 - b1*c2)  `div` det
   y = (a1*c2 - a2*c1)  `div` det

--convertRay :: (Int,Int) -> (Int,Int) -> (Int,Int,Int)
convertRay  (x, y) (dx, dy) = (a,b,c) 
  where 
    a = dy             -- (y+dy) - y  
    b = -dx            -- x - (x+dx)
    c = a*x+b*y
   
--convertLine :: (Int,Int) -> (Int,Int) -> (Int,Int,Int)
convertLine (x1,y1) (x2,y2) = (a,b,c) 
  where 
    a = y2 - y1 
    b = x1 - x2
    c = a*x1+b*y1 
    
    
----------------------------------------------------------------------------
-- intersection tests that are more specific to the task at hand  

{- 
intersectX :: Ray -> Line -> Maybe Vector2D 
intersectX (Ray r1 d1) (Line p1 p2) =  Just (fst p1,snd r1 + floori_ ysect  )	
  where	
    d       = fst p1 - fst r1
    divisor = if fst d1 == 0 then 0.0001 else fromIntegral (fst d1)
    ratio'  = fromIntegral (snd d1) / divisor 
    ratio   = if ratio' == 0.0 then  0.0001 else ratio'
    ysect   = fromIntegral d * ratio

intersectY :: Ray -> Line -> Maybe Vector2D 
intersectY (Ray r1 d1) (Line p1 p2) =  Just (fst r1 + floori_ xsect ,snd p1 )	
  where	
    d       = snd p1 - snd r1
    divisor = if snd d1 == 0 then 0.0001 else fromIntegral (snd d1)
    ratio'  = fromIntegral (fst d1) / divisor 
    ratio   = if ratio' == 0.0 then 0.0001 else ratio'
    xsect   = fromIntegral d * ratio
-} 
