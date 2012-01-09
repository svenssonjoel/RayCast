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



import Control.Monad
import Data.Array

import Data.Word
import Data.Bits


import SDLUtils 

import System.IO.Unsafe

import Foreign.Ptr
import Foreign.Storable 
import Foreign.Marshal.Array

import CExtras
import MathExtras
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
viewDistance   = floor (fromIntegral windowWidth * 0.6) -- 192 at 320  
walkSpeed      = wallWidth `div` 8

lightRadius    = 128.0


wallHeight, wallWidth :: Int 
wallHeight      = 256
wallWidth       = 256
gridMask        = negate (wallWidth - 1)

modMask         = 255

textureWidth, textureHeight :: Int 
-- Currently not used 
textureWidth    = 256 
textureHeight   = 256


viewerHeight    = wallHeight `div` 2
viewportCenterY = windowHeight `div` 2
viewportCenterX = windowWidth `div` 2

windowWidth     = 800  -- number of rays ! 
windowHeight    = 600



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
             then (rayX ray .&. gridMask) + wallWidth
             else (rayX ray .&. gridMask) - 1
    grid_y = if (posRayDy ray) 
             then (rayY ray .&. gridMask) + wallWidth
             else (rayY ray .&. gridMask) -1 
                  
    
    -- Create two lines for intersection test
    x_line = Line (fromIntegral grid_x,-10000) (fromIntegral grid_x,10000) 
    y_line = Line (-10000,fromIntegral grid_y) (10000,fromIntegral grid_y)  
    
    -- intersect ray with both vertical and horizontal line
    -- the closest one is used. 
    x_intersect = intersect ray x_line 
    y_intersect = intersect ray y_line
    
    ((px,py),dist,offs)  = 
      case (x_intersect,y_intersect) of 
        (Nothing,Nothing) -> error "Totally impossible" 
        (Just p, Nothing) -> (p, distance (rayStart ray) p,(snd p `mod` wallWidth) )
        (Nothing, Just p) -> (p, distance (rayStart ray) p,(fst p `mod` wallWidth) ) 
        (Just p, Just q)  -> 
          let d1 = distance (rayStart ray) p 
              d2 = distance (rayStart ray) q 
          in if d1 < d2 
             then (p,d1,(snd p `mod` wallWidth) ) 
             else (q,d2,(fst q `mod` wallWidth) ) 
    value = world !! (px `div` wallWidth, py `div` wallWidth)

     
-- TODO: improve on these (better names)   
    
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

mkRay :: Point2D -> Float -> Ray 
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
-- rendering routines 
renderView :: Array2D Int Int 
              -> Int 
              -> Int 
              -> Float 
              -> Surface 
              -> Surface -> IO ()
renderView world px py angle surf tex =  
    mapM_ (renderCol surf tex) distCol 
  where 
    -- avoid div by zero (but does it ever really happen?) 
    dists'   = results -- map (\(dist,i,x) -> (if dist == 0.0 then 0.1 else dist,i,x)) results 
    
    -- fixes the "fish eye" phenomenom    (*cos(angle)) 
    dists    = zipWith (\(dist,i,x) angle -> (dist*cos(angle),i,x)) dists' colAngles 
    distCol = zip dists [0..] 
    colAngles = [atan ((fromIntegral (col-viewportCenterX)) / 
                       (fromIntegral viewDistance)) 
                | col <- [0..windowWidth-1]
                ] 
    
    rays = map (\r -> mkRay (px,py) (r+angle)) colAngles
    
    results = map (castRay2 world 0.0) rays 


    
-- draw a single column into surf
renderCol surf tex ((dist,i,x),c) = 
  --vertLine c starty endy color surf
  -- texturedVLine c starty endy surf  x 0 64 tex
  -- texVLine c starty endy surf x 0 textureHeight tex
  texVLineLit c 
              starty 
              endy 
              surf 
              x 
              0 
              textureHeight 
              tex 
              (min 1.0 (lightRadius/dist)) 
  where 
    height = floor (fromIntegral (viewDistance * wallHeight) / (max dist 4) )
    starty = endy - height 
    endy   = floor (fromIntegral viewportCenterY + ((fromIntegral height) / 2)) --floor (fromIntegral (viewDistance * viewerHeight) / dist + fromIntegral viewportCenterY) 
     
----------------------------------------------------------------------------      
-- Cast for floors 
             
floorCast :: Array2D Int Int -> Float -> Int -> Int ->  Surface -> Surface -> IO ()              
floorCast world angle px py texture surf = 
    sequence_ [floorCastColumn world angle px py texture surf col
               | col <- [0..windowWidth-1]]
      
    

-- Approach to try next is to draw line by line  
-- This draws column by column
--  + a Hack to draw ceilings as well. 
-- TODO: Right now this completely ignores the map passed in
floorCastColumn :: Array2D Int Int -> Float -> Int -> Int -> Surface -> Surface -> Int -> IO ()
floorCastColumn world angle px py tex surf col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- castPtr `fmap` surfaceGetPixels tex
       
    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    radians = angle + columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX) / fromIntegral viewDistance)
    rows = [viewportCenterY..windowHeight-1]              


    ps = [(fromIntegral px  + distance * cos radians 
           ,fromIntegral py +  distance * sin radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
         
    ratioHeightRow row = fromIntegral viewerHeight / fromIntegral (row - viewportCenterY)
    rowDistance row = (ratioHeightRow row * fromIntegral viewDistance) / cos columnAngle     
         
    renderPoint :: Ptr Word32 -> Ptr Word32 -> Int -> Int -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        -- Read one Word32 instead of 4 word8
        p  <- peekElemOff tex t 
        
        let i = (min 1.0 (lightRadius/dist)) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ i * (fromIntegral p0) 
            p1' =  floor_ $ i * (fromIntegral p1) 
            p2' =  floor_ $ i * (fromIntegral p2) 
                        
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf r  p'     -- floor... 
        pokeElemOff surf r2 p'        
       
        
        where 
          t  = ((floor y .&. modMask) * textureWidth + (floor x .&. modMask))
          r  = (row * windowWidth + col)
          r2 = ((windowHeight-row) * windowWidth + col )
          
    {-                   
    renderPoint :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        -- Read one Word32 instead of 4 word8
        p0 <- peekElemOff tex t 
        p1 <- peekElemOff tex (t+1) 
        p2 <- peekElemOff tex (t+2) 
        p3 <- peekElemOff tex (t+3) 
        
        let i = (min 1.0 (lightRadius/dist)) 
        let p0' = floor $ i * (fromIntegral p0) 
            p1' = floor $ i * (fromIntegral p0) 
            p2' = floor $ i * (fromIntegral p0) 
        
        pokeElemOff surf r p0'      -- floor... 
        pokeElemOff surf (r+1) p1'  
        pokeElemOff surf (r+2) p2'  
        pokeElemOff surf (r+3) p3   
       
        pokeElemOff surf r2 p0'      -- ceiling... 
        pokeElemOff surf (r2+1) p1'   
        pokeElemOff surf (r2+2) p2'   
        pokeElemOff surf (r2+3) p3    
       
        
        where 
          t = 4 * ((floor y .&. modMask) * textureWidth + (floor x .&. modMask))
          r = 4 * (row * windowWidth + col)
          r2 =4 * ((windowHeight-row) * windowWidth + col )
      -}       
----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
  setVideoMode windowWidth windowHeight 32 []

  
  screen <- getVideoSurface
  toggleFullscreen screen
  
  putStrLn$ arr2dStr$ testLevelArr
  
  let pf = surfaceGetPixelFormat screen
      
  testTexture' <- loadBMP "textureLarge1.bmp" 
  testTexture <- convertSurface testTexture' pf [] 
  floorTex'   <- loadBMP "textureFloor1.bmp"            
  floorTex    <- convertSurface floorTex' pf [] 
                 
                 
  eventLoop screen testTexture floorTex
    (False,False,False,False) -- Keyboard state
    (0.0,7*wallWidth ,7*wallWidth)
  
  quit
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: Surface 
             -> Surface 
             -> Surface 
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int, Int) 
             -> IO ()
eventLoop screen texture fltex (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
      
  floor <- mapRGB pf 8 8 8     -- color of floors
  ceil  <- mapRGB pf 4 4 5  -- color of ceilings 
  
 
  -- draw single colored floor and ceilings (here use 320 for widht, inconsistent?)
  fillRect screen (Just (Rect 0 0 windowWidth (windowHeight `div` 2))) ceil    
  -- fillRect screen (Just (Rect 0 (windowHeight `div` 2) windowWidth windowHeight)) floor
  
  -- draw all the visible walls
  floorCast  testLevelArr r x y fltex screen
  renderView testLevelArr x y r screen texture
  -- TODO: order of arguments is messed up!
  
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

  unless b $ eventLoop screen texture fltex (up',down',left',right') (r',x',y')     
  
  
  -- very crude colision against walls added
  where 
    moveLeft  b (r,x,y) = if b then (r-0.04,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r+0.04,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (floor ((fromIntegral walkSpeed)*cos r))
        y' = y + (floor ((fromIntegral walkSpeed)*sin r))
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (floor ((fromIntegral walkSpeed)*cos r))
        y' = y - (floor ((fromIntegral walkSpeed)*sin r))
    movementAllowed (px,py) = testLevelArr !! (px `div` wallWidth,py `div` wallWidth) == 0
