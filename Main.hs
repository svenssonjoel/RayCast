{- 
  RayCasting (Like in the old Wolfenstein 3d game)  

  This is an attempt to Haskellify the code from 
  Christopher Lamptons Book "Gardens of Imagination" 
 
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
import qualified Prelude as P
import Graphics.UI.SDL as SDL



import Control.Monad
import Data.Array

import Data.Word
import Data.Bits
import Data.Int

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
            
testFloor = [[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3],
             [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1],
             [2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3]]

type MapType = Array2D Int32 Int32
type Array2D i e = Array i (Array i e)

testLevelArr :: Array2D Int32 Int32 
testLevelArr = listArray (0,15) (map (listArray (0,15)) testLevel)
testLevelFloorArr :: Array2D Int32 Int32 
testLevelFloorArr = listArray (0,15) (map (listArray (0,15)) testFloor)



(!!) arr (x,y) = (arr ! y) ! x 
arr2dStr arr = unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 

----------------------------------------------------------------------------
-- some constants
viewDistance   = floori_ (fromIntegral windowWidth * 0.6) -- 192 at 320  
walkSpeed      = wallWidth `div` 8

lightRadius    = 128.0


wallHeight, wallWidth :: Int32 
wallHeight      = 256
wallWidth       = 256
gridMask        = negate (wallWidth - 1) -- used to find gridlines
modMask         = 255                    -- used to get value `mod` 256 by an and operation

textureWidth, textureHeight :: Int32 
textureWidth    = 256 
textureHeight   = 256


viewerHeight    = wallHeight `div` 2
viewportCenterY = windowHeight `div` 2
viewportCenterX = windowWidth `div` 2

windowWidth     = 800  -- number of rays ! 
windowHeight    = 600

----------------------------------------------------------------------------
-- Slice : One slice of wall

data Slice = Slice {sliceTop :: Int32,
                    sliceBot :: Int32, 
                    sliceTex :: Int32,
                    sliceTexCol :: Int32,
                    sliceIntensity :: Float}

type Angle = Float 

---------------------------------------------------------------------------- 
-- castRay
castRay :: Array2D Int32 Int32 -> Point2D -> Angle -> Int32 -> Slice 
castRay world pos angle column = Slice top bot texValue texCol (min 1.0 (lightRadius/dist))  
  where 
    ray  = mkRay pos (angle - columnAngle)
    columnAngle = atan $ fromIntegral col / fromIntegral viewDistance
    top  = bot - height 
    bot  = floori_ $ fromIntegral viewportCenterY + (fromIntegral height / 2) 
    height = floori_ $ fromIntegral (viewDistance * wallHeight) / dist
    dist = dist' * cos columnAngle
    col = column - viewportCenterX
    (dist', texValue, texCol) = castRay2 world 0.0 ray 
 
   
    


----------------------------------------------------------------------------
-- castRay2
castRay2 :: Array2D Int32 Int32 -> Float -> Ray  -> (Float,Int32,Int32)
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
    x_line = Line (fromIntegral grid_x,0) (fromIntegral grid_x,1) 
    y_line = Line (0,fromIntegral grid_y) (1,fromIntegral grid_y)  
    
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
type Vector2D = (Int32,Int32) 
type Point2D  = (Int32,Int32)

data Ray     = Ray  Point2D Vector2D -- Point direction representation    

mkRay :: Point2D -> Float -> Ray 
mkRay p r    = Ray p (floori_ (-1024.0*sin r), floori_ (1024.0*cos r))  

data Line    = Line Point2D Point2D -- two points on the line  

distance :: Point2D -> Point2D -> Float
distance (x1, y1) (x2, y2) = 
  sqrt $ xd*xd+yd*yd
    where 
      xd = fromIntegral (x2 - x1)
      yd = fromIntegral (y2 - y1)

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


-- Intersection between ray and line. 
-- TODO: should there be a case for coincident ray/line 
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
-- rendering routines 
    
-- renderWalls, to replace renderView
renderWalls :: Array2D Int32 Int32 -> Point2D -> Angle -> [Surface] -> Surface -> IO [Slice]
renderWalls world pos angle textures surf = 
  do 
    zipWithM_ (drawSlice textures surf) [0..windowWidth-1] slices 
    return slices
  where 
    slices = map (castRay world pos angle)  [0..windowWidth-1]
    
drawSlice :: [Surface] -> Surface -> Int32 -> Slice -> IO () 
drawSlice textures surf col slice = 
  texVLineLit (fromIntegral col) 
              (fromIntegral (sliceTop slice)) 
              (fromIntegral (sliceBot slice)) 
              surf 
              (fromIntegral (sliceTexCol slice))
              0 
              (fromIntegral textureHeight) 
              (textures  P.!! (fromIntegral (sliceTex slice - 1)))
              (sliceIntensity slice) 
  
    
renderView :: Array2D Int32 Int32 
              -> Int32 
              -> Int32 
              -> Float 
              -> Surface 
              -> [Surface] -> IO ()
renderView world px py angle surf tex =  
    mapM_ (renderCol surf tex) distCol 
  where 
    dists'   =  results -- map (\(dist,i,x) -> (if dist == 0.0 then 0.1 else dist,i,x)) results 
    
    -- fixes the "fish eye" phenomenom    (*cos(angle)) 
    dists    = zipWith (\(dist,i,x) r -> (dist*cos(r),i,x)) dists' colAngles 
    distCol = zip dists [0..] 
    colAngles = [atan ((fromIntegral c) / 
                       (fromIntegral viewDistance)) 
                | col <- [0..windowWidth-1], let c = col-viewportCenterX
                ] 
    
    rays = map (\r -> mkRay (px,py) (r+angle)) colAngles
    
    results = map (castRay2 world 0.0) rays 


    
-- draw a single column into surf
renderCol surf tex ((dist,i,x),c) = 
  -- vertLine c starty endy color surf
  -- texturedVLine c starty endy surf  x 0 64 tex
  -- texVLine c starty endy surf x 0 textureHeight tex
  
  texVLineLit (fromIntegral c) 
              (fromIntegral starty) 
              (fromIntegral endy) 
              surf 
              (fromIntegral x) 
              0 
              (fromIntegral textureHeight) 
              (tex P.!! (fromIntegral i - 1))
              (min 1.0 (lightRadius/dist)) 

  where 
    height = floori_ (fromIntegral (viewDistance * wallHeight) / dist)
    starty = endy - height 
    endy   = floori_ (fromIntegral viewportCenterY + ((fromIntegral height) / 2)) --floor (fromIntegral (viewDistance * viewerHeight) / dist + fromIntegral viewportCenterY) 
     
----------------------------------------------------------------------------      
-- Cast for floors 
-- The slices are just there to be able to make some optimisations.              
newFloorCast :: MapType -> Point2D -> Angle -> [Slice] -> [Surface] -> Surface -> IO ()              
newFloorCast world pos angle slices textures surf =              
  zipWithM_ (newFloorCastColumn world pos angle textures surf) slices [0..windowWidth-1]
             
newFloorCastColumn :: MapType -> Point2D -> Float -> [Surface] -> Surface -> Slice -> Int32 -> IO ()
newFloorCastColumn world (px,py) angle tex surf slice col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- mapM ((return . castPtr) <=< surfaceGetPixels) tex

    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX) / fromIntegral viewDistance)
    
    -- only check floor where there are no walls (optimisation) 
    rows = [sliceBot slice..windowHeight-1]
    -- rows = [viewportCenterY+1..windowHeight-1]              


    ps = [(fromIntegral px - distance * sin radians 
          ,fromIntegral py + distance * cos radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
    
    ratioHeightRow row = fromIntegral viewerHeight / fromIntegral (row - viewportCenterY) 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral viewDistance / cos columnAngle
         
    renderPoint :: [Ptr Word32] -> Ptr Word32 -> Int32 -> Int32 -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        let (tx,ty) = (floori_ x `div` wallWidth, floori_ y `div` wallWidth) 
        
        p  <- peekElemOff (tex P.!! (fromIntegral (world !! (tx,ty)))) (fromIntegral t) 
        
        let i = (min 1.0 (lightRadius/dist)) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ i * (fromIntegral p0) 
            p1' =  floor_ $ i * (fromIntegral p1) 
            p2' =  floor_ $ i * (fromIntegral p2) 
                                
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf (fromIntegral r)  p'     -- floor... 
        pokeElemOff surf (fromIntegral r2) p'     -- ceiling...   
        
        where 
          t  = ((floori_ y .&. modMask) * textureWidth + (floori_ x .&. modMask))
          r  = (row * windowWidth + col)
          r2 = ((windowHeight-row-1) * windowWidth + col )
    
  
  
  
floorCast :: Array2D Int32 Int32 -> Int32 -> Int32 -> Float -> Surface -> [Surface] -> IO ()              
floorCast world px py angle surf texture = 
    sequence_ [floorCastColumn world px py angle surf texture col
               | col <- [0..windowWidth-1]]
      
-- Approach to try next is to draw line by line  
-- This draws column by column
--  + a Hack to draw ceilings as well. 
-- TODO: Right now this completely ignores the map passed in
floorCastColumn :: Array2D Int32 Int32 -> Int32 -> Int32 -> Float -> Surface -> [Surface] -> Int32 -> IO ()
floorCastColumn world px py angle surf tex col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- mapM (\s -> do ptr <- surfaceGetPixels s; return (castPtr ptr)) tex
       
    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    radians = angle + columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX) / fromIntegral viewDistance)
    rows = [viewportCenterY+1..windowHeight-1]              


    ps = [(fromIntegral px - distance * sin radians 
          ,fromIntegral py + distance * cos radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
    
    ratioHeightRow row = fromIntegral viewerHeight / fromIntegral (row - viewportCenterY) 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral viewDistance / cos columnAngle
         
    renderPoint :: [Ptr Word32] -> Ptr Word32 -> Int32 -> Int32 -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        -- Read one Word32 instead of 4 word8
        -- why does this produce visibly ok results with "mod 16" ?? 
        -- The mod 16 kicks in when floor outside of the map is being "cast"
        let (tx,ty) = (floori_ x `div` wallWidth `mod` 16, floori_ y `div` wallWidth `mod` 16) 
        
        
        
        p  <- peekElemOff (tex P.!! (fromIntegral (world !! (tx,ty)))) (fromIntegral t) 
        
        let i = (min 1.0 (lightRadius/dist)) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ i * (fromIntegral p0) 
            p1' =  floor_ $ i * (fromIntegral p1) 
            p2' =  floor_ $ i * (fromIntegral p2) 
                                
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf (fromIntegral r)  p'     -- floor... 
        pokeElemOff surf (fromIntegral r2) p'     -- ceiling...   
        
        where 
          t  = ((floori_ y .&. modMask) * textureWidth + (floori_ x .&. modMask))
          r  = (row * windowWidth + col)
          r2 = ((windowHeight-row) * windowWidth + col )
    

----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
  setVideoMode (fromIntegral windowWidth) 
               (fromIntegral windowHeight) 32 []

  
  screen <- getVideoSurface
  -- toggleFullscreen screen
  
  putStrLn$ arr2dStr$ testLevelArr
  
  let pf = surfaceGetPixelFormat screen
      
  --testTexture' <- loadBMP "Data/textureLarge1.bmp" 
  --testTexture <- convertSurface testTexture' pf [] 
  --floorTex'   <- loadBMP "Data/floor1.bmp"            
  --floorTex    <- convertSurface floorTex' pf [] 
                 
  wallTextures <- sequence [conv pf =<< loadBMP "Data/textureLarge1.bmp"
                           ,conv pf =<< loadBMP "Data/textureLarge2.bmp"]
                 
  floorTextures <- sequence [conv pf =<< loadBMP "Data/floor1.bmp"
                            ,conv pf =<< loadBMP "Data/floor2.bmp"
                            ,conv pf =<< loadBMP "Data/floor3.bmp"
                            ,conv pf =<< loadBMP "Data/floor4.bmp" ]


                 
  eventLoop screen floorTextures wallTextures -- testTexture floorTex
    (False,False,False,False) -- Keyboard state
    (0.0,7*wallWidth ,7*wallWidth)
  
  quit
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: Surface 
             -> [Surface] 
             -> [Surface] 
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int32, Int32) 
             -> IO ()
eventLoop screen floorTextures wallTextures(up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
  
  -- draw all the visible walls
  -- floorCast  testLevelFloorArr x y r screen floorTextures
  
  slices <- renderWalls testLevelArr (x,y) r wallTextures screen
  newFloorCast testLevelFloorArr (x,y) r slices floorTextures screen
  
  
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

  unless b $ eventLoop screen floorTextures wallTextures (up',down',left',right') (r',x',y')     
  
  
  -- very crude colision against walls added
  where 
    moveLeft  b (r,x,y) = if b then (r+0.04,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r-0.04,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y + (floori_ ((fromIntegral walkSpeed)*cos r))
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (floori_ ((fromIntegral walkSpeed)*sin r))
        y' = y - (floori_ ((fromIntegral walkSpeed)*cos r))
    movementAllowed (px,py) = testLevelArr !! (px `div` wallWidth,py `div` wallWidth) == 0
