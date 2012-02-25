{- 2012 Joel Svensson -} 

module Engine.Math where 

import Data.Int

import Foreign.C.Types
import Foreign.Storable

import MathExtras

----------------------------------------------------------------------------
-- Angles, Points, Vectors, View


type View = (Point2D, Angle) 

type Angle = Float 

data Point2D = Point2D {point2DGetX :: !Float, 
                        point2DGetY :: !Float}
              deriving (Eq,Show)
                       
mkPoint (x,y) = Point2D x y                       
data Vector2D = Vector2D {vector2DGetX :: !Float, 
                          vector2DGetY :: !Float}
               deriving (Eq,Show)
                        
mkVector (x,y) = Vector2D x y                         

data Dims2D = Dims2D {dims2DGetW :: !Int32,
                      dims2DGetH :: !Int32} 
              deriving (Eq,Show)
mkDims (w,h) = Dims2D w h

instance Storable Point2D where
  sizeOf _  = sizeOf (undefined :: Float) * 2 
  alignment _ = 4
  peek p = do 
    x <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat) 
    y <- realToFrac `fmap` (peekByteOff p 4 :: IO CFloat) 
    return $ Point2D x y 
  poke p (Point2D x y) = do 
    pokeByteOff p 0 (realToFrac x :: CFloat) 
    pokeByteOff p 4 (realToFrac y :: CFloat)

instance Num Point2D where 
  (+) (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1+x2) (y1+y2) 
  (-) (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1-x2) (y1-y2) 
  (*) (Point2D x1 y1) (Point2D x2 y2) = Point2D (x1*x2) (y1*y2) 
  abs = undefined
  signum = undefined 
  fromInteger i = Point2D (fromInteger i) 0 

instance Storable Vector2D where
  sizeOf _  = sizeOf (undefined :: Int32) * 2 
  alignment _ = 4
  peek p = do 
    x <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat)
    y <- realToFrac `fmap` (peekByteOff p 4 :: IO CFloat) 
    return $ Vector2D x y 
  poke p (Vector2D x y) = do 
    pokeByteOff p 0 (realToFrac x :: CFloat) 
    pokeByteOff p 4 (realToFrac y :: CFloat)


instance Num Vector2D where 
  (+) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1+x2) (y1+y2) 
  (-) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1-x2) (y1-y2) 
  (*) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1*x2) (y1*y2) 
  abs = undefined
  signum = undefined 
  fromInteger i = Vector2D (fromInteger i) 0 
  
instance Storable Dims2D where
  sizeOf _  = sizeOf (undefined :: Int32) * 2 
  alignment _ = 4
  peek p = do 
    w <- fromIntegral `fmap` (peekByteOff p 0 :: IO CInt)
    h <- fromIntegral `fmap` (peekByteOff p 4 :: IO CInt) 
    return $ Dims2D w h 
  poke p (Dims2D w h) = do 
    pokeByteOff p 0 (fromIntegral w :: CInt) 
    pokeByteOff p 4 (fromIntegral h :: CInt)

vecAdd = (+)
vecSub = (-) 
vecDot (Vector2D x1 y1) (Vector2D x2 y2) = x1*x2 + y1*y2

distance :: Point2D -> Point2D -> Float 
distance p1 p2 = 
  sqrt $ xd*xd+yd*yd 
  where 
    x1 = point2DGetX p1;
    y1 = point2DGetY p1; 
    x2 = point2DGetX p2;
    y2 = point2DGetY p2; 
    
    xd = x2 - x1 
    yd = y2 - y1


-- assumes that p is on the line!
distanceAlongLine p (Line s _) 
  = distance p s               

----------------------------------------------------------------------------
-- Rays 

data Ray = Ray {rayStart  :: !Point2D,
                rayDeltas :: !Vector2D}  
           -- point direction repr 
           
instance Storable Ray where 
  sizeOf (Ray p d)  = sizeOf p + sizeOf d 
  alignment _ = 4 -- Again ?? 
  peek ptr = do 
    p <- (peekByteOff ptr 0 :: IO Point2D)
    d <- (peekByteOff ptr (sizeOf (undefined :: Point2D)) :: IO Vector2D) 
    return $ Ray p d 
  poke ptr (Ray p d) = do 
    pokeByteOff ptr 0 p
    pokeByteOff ptr (sizeOf (undefined :: Point2D)) d



mkRay :: Point2D -> Angle -> Ray 
mkRay p r = Ray p 
                (mkVector (-1024.0*sin r,
                            1024.0*cos r))

posRayDx  r = rayDx r > 0 
posRayDy  r = rayDy r > 0 

rayDx r = vector2DGetX $ rayDeltas r 
rayDy r = vector2DGetY $ rayDeltas r 

rayX  r = point2DGetX $ rayStart r 
rayY  r = point2DGetY $ rayStart r


----------------------------------------------------------------------------
-- Lines 

data Line = Line !Point2D !Point2D -- 2 points on the line  

instance Storable Line where 
  sizeOf (Line p1 p2)  = sizeOf p1 + sizeOf p2
  alignment _ = 4 -- Again ?? 
  peek ptr = do 
    p1 <- (peekByteOff ptr 0 :: IO Point2D)
    p2 <- (peekByteOff ptr (sizeOf (undefined :: Point2D)) :: IO Point2D) 
    return $ Line p1 p2 
  poke ptr (Line p1 p2) = do 
    pokeByteOff ptr 0 p1
    pokeByteOff ptr (sizeOf (undefined :: Point2D))  p2

mkLine = Line 

----------------------------------------------------------------------------
-- 

-- intersect ray against a line segment.
intersectSeg :: Ray -> Line -> Maybe Point2D 
intersectSeg ray line = onSeg line $intersect ray line
  where     
    onSeg line mp = 
      case mp of 
        Nothing -> Nothing 
        (Just p) -> if pointOnSeg p line
                    then mp 
                    else Nothing     
                         
pointOnSeg (Point2D px py) 
           (Line (Point2D x1 y1) (Point2D x2 y2)) = 
  (px >= min x1 x2 && px <= max x1 x2 && 
   py >= min y1 y2 && py <= max y1 y2) 
      

-- intersect ray against "infinite" line.
intersect :: Ray -> Line -> Maybe Point2D 
intersect (Ray p1 d1) (Line p2 d2) = if det == 0 
                                     then Nothing 
                                     else (Just (mkPoint (x,y))) 
 where  
   (a1,b1,c1) = convertRay p1 d1
   (a2,b2,c2) = convertLine p2 d2 
   det = a1*b2 - a2*b1
   
   x = (b2*c1 - b1*c2) / det
   y = (a1*c2 - a2*c1) / det

convertRay :: Point2D -> Vector2D -> (Float,Float,Float)
convertRay  (Point2D x y) (Vector2D dx dy) = (a,b,c) 
  where 
    a = dy             -- (y+dy) - y  
    b = -dx            -- x - (x+dx)
    c = a*x+b*y
   
convertLine :: Point2D -> Point2D -> (Float,Float,Float)
convertLine (Point2D x1 y1) (Point2D x2 y2) = (a,b,c) 
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


----------------------------------------------------------------------------
-- vectors of 2 floats 

data Vector3Df = Vector3Df !Float !Float !Float 
               deriving (Eq,Show)
mkVector3Df = Vector3Df

instance Num Vector3Df where 
  (+) (Vector3Df x y z) (Vector3Df u v w) = Vector3Df (x+u) (y+v) (z+w) 
  (-) (Vector3Df x y z) (Vector3Df u v w) = Vector3Df (x-u) (y-v) (z-w) 
  (*) (Vector3Df x y z) (Vector3Df u v w) = Vector3Df (x*u) (y*v) (z*w) 
  abs = undefined
  signum = undefined 
  fromInteger = undefined
  
instance Storable Vector3Df where
  sizeOf _  = sizeOf (undefined :: Float) * 3 
  alignment _ = 4 -- ?? 
  peek p = do 
    x <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat) 
    y <- realToFrac `fmap` (peekByteOff p s :: IO CFloat) 
    z <- realToFrac `fmap` (peekByteOff p (s+s) :: IO CFloat)
    return $ mkVector3Df x y z 
    where s = sizeOf (undefined :: CFloat) 
  poke p (Vector3Df x y z) = do 
    pokeByteOff p 0 (realToFrac x :: CFloat) 
    pokeByteOff p s (realToFrac y :: CFloat)
    pokeByteOff p (s+s) (realToFrac z :: CFloat)
    where s = sizeOf (undefined :: CFloat)
    
    
----------------------------------------------------------------------------
-- line where endpoints are real-valued 

data RealLine = RealLine (Float,Float) (Float,Float) 
   deriving (Eq,Show) 
            
mkRealLine = RealLine 

instance Storable RealLine where 
  sizeOf _ = sizeOf (undefined :: Float) * 4
  alignment _ = 4 -- ?  
  peek p = do 
    x1 <- realToFrac `fmap` (peekByteOff p 0 :: IO CFloat) 
    y1 <- realToFrac `fmap` (peekByteOff p s :: IO CFloat)
    x2 <- realToFrac `fmap` (peekByteOff p (s+s) :: IO CFloat)
    y2 <- realToFrac `fmap` (peekByteOff p (s+s+s) :: IO CFloat)
    return $ mkRealLine (x1,y1) (x2,y2) 
    where s = sizeOf (undefined :: CFloat)
  poke p (RealLine (x1,y1) (x2,y2)) = do 
    pokeByteOff p 0 (realToFrac x1 :: CFloat) 
    pokeByteOff p s (realToFrac y1 :: CFloat)
    pokeByteOff p (s+s) (realToFrac x2 :: CFloat)
    pokeByteOff p (s+s+s) (realToFrac y2 :: CFloat)    
    where s = sizeOf (undefined :: CFloat)
    