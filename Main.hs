{- 
  RayCasting (Like in the old Wolfenstein 3d game)  

  This is an attempt to Haskellify the code from 
  Christopher Lamptons Book "Gardens of Imagination" 
  
  TODO 
   * Clean up code (make use of Haskell!) 
   * Texturing 
   * Improve modularity  
   * Collision detection
   * There is some problem with the line intersection code. 
     

  This is also an exercise in using SDL. 
  
  Early problems in using SDL is. 
    - SDL is low level! 
       It expects you to implement your own graphics routines 
       on the supplied "surfaces" and pixels thereof. This is Ok 
       in C where it possible to write your own optimized drawing routines 
       but I suspect that it is, from that point of view,  trickier in Haskell. 
       + If SDL had optimized "larger" building blocks like stretchBlit it would help. 
         
-} 

module Main where 

import Prelude hiding ((!!))
import Graphics.UI.SDL as SDL

import Data.Bits

import Control.Monad
import Data.Array

import Data.Word

import SDLUtils 

import Debug.Trace
 

----------------------------------------------------------------------------
--
    
testLevel = [[1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2], 
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,2,1,2,1,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,2,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,2,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1],
             [1,0,0,0,0,0,0,0,1,2,1,2,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2]] 


type Array2D i e = Array i (Array i e)

testLevelArr :: Array2D Int Int 
testLevelArr = listArray (0,15) (map (listArray (0,15)) testLevel)

(!!) arr (x,y) = (arr ! y) ! x  
arr2dStr arr = unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 

----------------------------------------------------------------------------
-- some constants
viewDistance   = 192 
wallHeight     = 64 
viewerHeight   = 32
viewportCenter = 100

windowWidth    = 320 
windowHeight   = 200

----------------------------------------------------------------------------
-- CastRay is a quite direct translation from the C code in the Book
{- 
castRay :: Array2D Int Int -> Int -> Int -> Int -> Int -> Float -> (Float,Int)
castRay world x y cx cy colAngle = if (xp < 0 || xp > 15 || yp < 0 || yp > 15) 
                                      then error "You are outside the world!" -- (100.0,1)  -- just make something up if value > 0 
                                      else 
                                        if (value > 0) 
                                        then (distance',value)  
                                        else castRay world x y nx ny colAngle  
                                   
  where 
    xdiff = floor (1024*(cos colAngle))
    ydiff = floor (1024*(sin colAngle))
    grid_x = if (xdiff > 0) 
             then (cx .&. 0xffc0) + 64
             else (cx .&. 0xffc0) - 1
    grid_y = if (ydiff > 0) 
             then (cy .&. 0xffc0) + 64
             else (cy .&. 0xffc0) -1 
    horizontal_crossing_at_x = (fromIntegral grid_x) 
    horizontal_crossing_at_y = (fromIntegral cy) + (slope xdiff ydiff) * (fromIntegral (grid_x - cx))  
    vertical_crossing_at_x   = (fromIntegral cx) + (fromIntegral (grid_y-cy)) / (slope xdiff ydiff)
    vertical_crossing_at_y   = (fromIntegral grid_y)
    horizontal_dist = dist (horizontal_crossing_at_x - (fromIntegral x), 
                            horizontal_crossing_at_y - (fromIntegral y)) 
    vertical_dist   = dist (vertical_crossing_at_x - (fromIntegral x),                  
                            vertical_crossing_at_y - (fromIntegral y))
    (xp,yp,nx,ny)  = if (horizontal_dist < vertical_dist) 
                     then (floor (horizontal_crossing_at_x / 64), 
                           floor (horizontal_crossing_at_y / 64),
                           floor horizontal_crossing_at_x,
                           floor horizontal_crossing_at_y) 
                     else (floor (vertical_crossing_at_x / 64),      
                           floor (vertical_crossing_at_y / 64),
                           floor vertical_crossing_at_x,
                           floor vertical_crossing_at_y) 
    distance' = min horizontal_dist vertical_dist 
    value = world !! (xp,yp) 
    
slope :: (Integral a, Fractional b, Ord b)  => a -> a -> b 
slope dx dy = if sl == 0.0 then 0.0001 else sl   
  where 
    sl = ((fromIntegral dy) / (fromIntegral dx))

dist (xd,yd) = max 1 (sqrt (xd*xd+yd*yd))
-}
---------------------------------------------------------------------------- 
-- castRay2 
castRay2 :: Array2D Int Int -> (Int,Ray) -> (Float,Int)
castRay2 world (id,ray) =  -- the ID is for debuging 
  if (floor (px/64) > 15 || floor (px/64) < 0 || floor (py/64) > 15 || floor (py/64) < 0) 
  then (200,1)
  else                                                                         
    if (value > 0)  
    then (dist,value) 
    else 
      let (d,v) = castRay2 world (id,(Ray (px ,py) (rayDeltas ray)))
      in  (dist+d,v)
        
  where 
    -- TODO: grid_x and grid_y is problematic in this setting
    -- because of how I use 
    grid_x = if (posRayDx ray) 
             then ((floor (rayX ray) :: Int) .&. 0xffc0) + 64
             else ((floor (rayX ray) :: Int) .&. 0xffc0) - 1
    grid_y = if (posRayDy ray) 
             then ((floor (rayY ray) :: Int) .&. 0xffc0) + 64
             else ((floor (rayY ray) :: Int) .&. 0xffc0) -1 
                  
    -- For some reason adding small amounts here (to the x_line and y_line direction)               
    -- has same effect as the changes described in the "intersect" function
    x_line = Line (fromIntegral grid_x,0) (0.0,1.0) 
    y_line = Line (0,fromIntegral grid_y) (1.0,0.0)  
    
    -- Does the intersection code give wrong results for 
    -- vertical and horizontal lines ? 
    x_intersect = intersect ray x_line 
    y_intersect = intersect ray y_line
    
    ((px,py),dist)  = 
      case (x_intersect,y_intersect) of 
        (Nothing,Nothing) -> error "Totally impossible" 
        (Just p, Nothing) -> (p, distance (rayStart ray) p)
        (Nothing, Just p) -> (p, distance (rayStart ray) p) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 < d2 
             then (p,d1) 
             else (q,d2) 
    value = trace (show (id,(floor px `div` 64), (floor py `div` 64))) $ world !! (floor px `div` 64, floor py `div` 64)
     
    
posRayDx  (Ray _ (dx,_)) = dx > 0   
posRayDy  (Ray _ (_,dy)) = dy > 0 
rayX      (Ray (x,_) _) = x
rayY      (Ray (_,y) _) = y 
rayDx     (Ray _ (dx,_)) = dx 
rayDy     (Ray _ (_,dy)) = dy 
rayStart  (Ray s _) = s 
rayDeltas (Ray _ d) = d 

---------------------------------------------------------------------------- 
-- 
type Vector2D = (Float,Float) 

data Ray     = Ray  Vector2D Vector2D -- Point direction representation    
data Line    = Line Vector2D Vector2D -- point direction representation 


distance :: Vector2D -> Vector2D -> Float 
distance (x1, y1) (x2, y2) = 
  sqrt (xd*xd+yd*yd)
    where 
      xd = x2 - x1 
      yd = y2 - y1 


intersect :: Ray -> Line -> Maybe Vector2D 
intersect (Ray p1 d1) (Line p2 d2) = if det == 0.0 
                                     then Nothing 
                                     else (Just (x, y)) 
 where  
   (a1,b1,c1) = convertLine p1 d1
   (a2,b2,c2) = convertLine p2 d2 
   det = a1*b2 - a2*b1
   
   x = (b2*c1 - b1*c2) / det 
   y = (a1*c2 - a2*c1) / det
   
convertLine (x1, y1) (x2, y2) = (a,b,c) 
  where 
    
    --a = y2 
    --b = -x2
    --c = a*x1+b*y1     
    
    --Strange. I think above and below are "equal" 
    --but using the above leads to infinite loop "somewhere"
    --  + The loop is in CastRay2 
     
    a = (y1+y2) - y1  
    b = x1 - (x1+x2)  
    c = a*x1+b*y1     
   


----------------------------------------------------------------------------
-- rendering routines 
renderView world px py angle surf =  
    mapM_ (renderCol surf) distCol 
  where 
    dists''  = results 
    dists'   = map (\(x,y) -> (if x == 0 then 1 else x,y)) dists''
    dists    = zipWith (\(x,y) angle -> (x*cos(angle),y)) dists' colAngles 
    distCol = zip dists [0..] 
    colAngles = [atan ((fromIntegral (col-160)) / viewDistance) | col <- [0..319]] 
    -- colAngles = [atan ((fromIntegral (col-160)) / viewDistance) | col <- [0..799]] 
    rays = map (+angle) colAngles 
    --results = map (castRay world px py px py) rays                        
    
    rays' = map (\r -> (cos r,sin r)) rays
    rays''  = map (\deltas -> Ray (px,py) deltas) rays'  
    results = map (castRay2 world) (zip [0..] rays'')   

-- draw a single column into surf
renderCol surf ((dist,i),c) = 
  vertLine c starty endy color surf
    where 
      color = 
          -- TODO: Cheating here with the colors
          case i of 
            1 -> (128,0,0) 
            2 -> (255,0,128)
      height = floor (viewDistance * wallHeight / dist)
      starty = max 0 (endy - height)
      endy   = min 199 (floor (viewDistance * viewerHeight / dist + viewportCenter))
      -- endy   = min 599 (floor (viewDistance * viewerHeight / dist + viewportCenter))
----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
  --setVideoMode 320 200 32 [] 
  setVideoMode windowWidth windowHeight 32 []
  
  screen <- getVideoSurface
  putStrLn$ arr2dStr$ testLevelArr
  
  eventLoop screen (0.0,fromIntegral (7*64+32) ,fromIntegral (7*64+32))
  
  quit
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: Surface -> (Float,Float, Float) -> IO ()
eventLoop screen (r,x,y) = do 
  
  
  let pf = surfaceGetPixelFormat screen
  
  floor <- mapRGB pf 32 64 32 
  ceil  <- mapRGB pf 128 128 128  
  
 
  -- draw single colored floor and ceilings (here use 320 for widht, inconsistent?)
  fillRect screen (Just (Rect 0 0 windowWidth (windowHeight `div` 2))) ceil    
  fillRect screen (Just (Rect 0 (windowHeight `div` 2) windowWidth windowHeight)) floor
  
  
  -- draw all the visible walls
  --renderView testLevelArr (round x) (round y) r screen
  renderView testLevelArr x y r screen

  SDL.flip screen
  
  -- process events 
  e <- pollEvent
  
  let (r',x',y',b) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> (r-0.1,x,y,False)
              SDLK_RIGHT -> (r+0.1,x,y,False)
              SDLK_UP    -> 
                let 
                    dx = 32 * cos r
                    dy = 32 * sin r
                in (r,x+dx,y+dy,False)
              SDLK_DOWN  -> 
                let 
                    dx = 32 * cos r
                    dy = 32 * sin r
                in (r,x-dx,y-dy,False)
                
                
              otherwise  -> (r,x,y,False)
          Quit -> (r,x,y,True) -- quit 
          otherwise -> (r,x,y,False)
  
  unless b $ eventLoop screen (r',x',y')     
  
  