{-# OPTIONS_GHC -fspec-constr-count=16  #-}

{- 
  RayCasting (Like in the old Wolfenstein 3d game)  

  This is an attempt to Haskellify the code from 
  Christopher Lamptons Book "Gardens of Imagination" 
  
  TODO 
   * Clean up code (make use of Haskell!) 
   * Improve modularity  
   * Collision detection
   * There is some problem with the line intersection code. 
   * Right now the line intersection code is very sensitive      
     and I fear it might break down.
     + The line intersection code seems to have problems 
       with vertical and horizontal lines... 
   * If a ray fails to hit any wall (which should be rare) 
     it should be quite ok to just use the result of a neighbouring 
     ray. 
   * If a "level" is correct no ray should ever completely miss all walls. 
     (And if it does something is wrong in the line-line intersection calculation) 


  **************************************
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

import System.IO.Unsafe

import Foreign.Ptr

import CExtras

----------------------------------------------------------------------------
--
    
testLevel = [[1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2], 
             [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
             [1,0,1,0,0,0,0,0,2,1,2,1,0,0,0,1],
             [1,0,2,0,0,0,0,0,0,0,0,2,0,0,0,1],
             [1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1],
             [1,0,1,0,1,2,0,0,0,0,0,2,0,0,0,1],
             [1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,1],
             [1,0,2,0,0,2,0,0,1,2,1,2,0,0,0,1],
             [1,0,2,2,1,1,0,0,0,0,0,0,0,0,0,1],
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
-- castRay2 
castRay2 :: Array2D Int Int -> Float -> Ray  -> (Float,Int,Int)
castRay2 world accDist ray = 
    if (value > 0) -- ray has struck solid wall
    then (accDist+dist,value,offs) 
    else 
      -- Continue along the ray 
      castRay2 world (accDist+dist) (Ray (px ,py) (rayDeltas ray))

        
  where 
    grid_x = if (posRayDx ray) 
             then (rayX ray .&. 0xffc0) + 64
             else (rayX ray .&. 0xffc0) - 1
    grid_y = if (posRayDy ray) 
             then (rayY ray .&. 0xffc0) + 64
             else (rayY ray .&. 0xffc0) -1 
                  
    -- The gridlines are tweaked slightly to not be perfectly horizontal or vertical. 
    -- those two cases seem to upset the ray/line intersection test. 
    x_line = Line (fromIntegral grid_x,-10000) (fromIntegral grid_x,10000) 
    y_line = Line (-10000,fromIntegral grid_y) (10000,fromIntegral grid_y)  
    
    -- Does the intersection code give wrong results for 
    -- vertical and horizontal lines ? 
    x_intersect = intersect ray x_line 
    y_intersect = intersect ray y_line
    
    ((px,py),dist,offs)  = 
      case (x_intersect,y_intersect) of 
        (Nothing,Nothing) -> error "Totally impossible" 
        (Just p, Nothing) -> (p, distance (rayStart ray) p,(snd p `mod` 64) )
        (Nothing, Just p) -> (p, distance (rayStart ray) p,(fst p `mod` 64) ) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 < d2 
             then (p,d1,(snd p `mod` 64) ) 
             else (q,d2,(fst q `mod` 64) ) 
    value = world !! (px `div` 64, py `div` 64)

     
    
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
type Vector2D = (Int,Int) 
type Point2D  = (Int,Int)

data Ray     = Ray  Point2D Vector2D -- Point direction representation    
mkRay p r    = Ray p (floor(1024.0*cos r), floor (1024.0*sin r))  

data Line    = Line Point2D Point2D  -- Two points on line representation  


distance :: Vector2D -> Vector2D -> Float
distance (x1, y1) (x2, y2) = 
  sqrt $ fromIntegral (xd*xd+yd*yd)
    where 
      xd = x2 - x1 
      yd = y2 - y1 

-- Intersection between ray and line. 
-- TODO: should there be a case for coincident ray/line
intersect :: Ray -> Line -> Maybe Vector2D 
intersect (Ray p1 d1) (Line p2 d2) = if det == 0 
                                     then Nothing 
                                     else (Just (x, y)) 
 where  
   (a1,b1,c1) = convertRay p1 d1
   (a2,b2,c2) = convertLine p2 d2 
   det = a1*b2 - a2*b1
   
   x = (b2*c1 - b1*c2)  `div` det 
   y = (a1*c2 - a2*c1)  `div` det

convertRay  (x, y) (dx, dy) = (a,b,c) 
  where 
    a = dy             -- (y+dy) - y  
    b = -dx            -- x - (x+dx)
    c = a*x+b*y
   
convertLine (x1,y1) (x2,y2) = (a,b,c) 
  where 
    a = y2 - y1 
    b = x1 - x2
    c = a*x1+b*y1

----------------------------------------------------------------------------
-- rendering routines 
renderView world px py angle surf tex =  
    mapM_ (renderCol surf tex) distCol 
  where 
    -- avoid div by zero (but does it ever really happen?) 
    dists'   = results -- map (\(dist,i,x) -> (if dist == 0.0 then 0.1 else dist,i,x)) results 
    
    -- fixes the "fish eye" phenomenom    (*cos(angle)) 
    dists    = zipWith (\(dist,i,x) angle -> (dist*cos(angle),i,x)) dists' colAngles 
    distCol = zip dists [0..] 
    colAngles = [atan ((fromIntegral (col-160)) / viewDistance) | col <- [0..319]] 
    
    rays = map (\r -> mkRay (px,py) (r+angle)) colAngles
    
    results = map (castRay2 world 0.0) rays 



-- draw a single column into surf
renderCol surf tex ((dist,i,x),c) = 
  --vertLine c starty endy color surf
  -- texturedVLine c starty endy surf  x 0 64 tex
  texVLine c starty endy surf x 0 64 tex
    where 
      height = floor (viewDistance * wallHeight / dist)
      starty = endy - height 
      endy   = floor (viewDistance * viewerHeight / dist + viewportCenter) 
     
      
----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
  setVideoMode windowWidth windowHeight 32 []

  
  screen <- getVideoSurface
  toggleFullscreen screen
  
  putStrLn$ arr2dStr$ testLevelArr
  
  let pf = surfaceGetPixelFormat screen
  testTexture' <- loadBMP "texture2.bmp" 
  testTexture <- convertSurface testTexture' pf [] 
                 
  eventLoop screen testTexture
    (False,False,False,False) -- Keyboard state
    (0,7*64+32 ,7*64+32)
  
  quit
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: Surface 
             -> Surface 
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int, Int) 
             -> IO ()
eventLoop screen texture (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
      
  floor <- mapRGB pf 32 64 32     -- color of floors
  ceil  <- mapRGB pf 128 128 128  -- color of ceilings 
  
 
  -- draw single colored floor and ceilings (here use 320 for widht, inconsistent?)
  fillRect screen (Just (Rect 0 0 windowWidth (windowHeight `div` 2))) ceil    
  fillRect screen (Just (Rect 0 (windowHeight `div` 2) windowWidth windowHeight)) floor
  
  -- draw all the visible walls
  renderView testLevelArr x y r screen texture

  SDL.flip screen
  
  -- process events 
  e <- pollEvent
  
  
  let (up',down',left',right',b) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT    -> (up,down,True,right,False)
              SDLK_RIGHT   -> (up,down,left,True,False)
              SDLK_UP      -> (True,down,left,right,False)
              SDLK_DOWN    -> (up,True,left,right,False)
              SDLK_ESCAPE  -> (up,down,left,right,True)
              otherwise    -> (up,down,left,right,False)
          (KeyUp k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> (up,down,False,right,False)
              SDLK_RIGHT -> (up,down,left,False,False)
              SDLK_UP    -> (False,down,left,right,False)
              SDLK_DOWN  -> (up,False,left,right,False)
              otherwise  -> (up,down,left,right,False)
          Quit -> (up,down,left,right,True) 
          otherwise -> (up,down,left,right,False)
  
  let (r',x',y') = (moveLeft left' . moveRight right' . moveUp up' . moveDown down') (r,x,y) 

  unless b $ eventLoop screen texture (up',down',left',right') (r',x',y')     
  
  
  -- very crude colision against walls added
  where 
    moveLeft  b (r,x,y) = if b then (r-0.02,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r+0.02,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (floor (4*cos r))
        y' = y + (floor (4*sin r))
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (floor (4*cos r))
        y' = y - (floor (4*sin r))
    movementAllowed (px,py) = testLevelArr !! (px `div` 64,py `div` 64) == 0
