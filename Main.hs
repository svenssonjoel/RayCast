{- 
  2012 Joel Svensson   


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

import Engine.RayCast
import Engine.Math
import Engine.Map
import Engine.Render

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

testLevelArr :: Array2D Int32 Int32 
testLevelArr = listArray (0,15) (map (listArray (0,15)) testLevel)
testLevelFloorArr :: Array2D Int32 Int32 
testLevelFloorArr = listArray (0,15) (map (listArray (0,15)) testFloor)


----------------------------------------------------------------------------
-- Constants 

walkSpeed      = 32 

-- This should be part of the "rendering settings"
-- or attached to texture objects somehow. 
textureWidth, textureHeight :: Int32 
textureWidth    = 256 
textureHeight   = 256


-- Paste from RayPortal
----------------------------------------------------------------------------
-- sprites (a movable or stationary object in the 2world) 
data Sprite = Sprite { spritePos       :: Point2D,      -- world x,y pos 
                       spriteElevation :: Float,        -- height above ground (z) 
                       spriteDims      :: (Float,Float),-- Base size  
                       spriteTexture   :: Surface}
                       

-- transform a world object into a screen object. 
-- ignoring elevation from floor currently. 
viewTransformSprite :: ViewConfig -> View -> Sprite -> Maybe RItem
viewTransformSprite vc (viewPos,viewAngle) spr  
  | ry >= 0 =
    Just $ RItem (projx_,viewportCenterY vc -(mh `div` 2)) 
                 (mw,mh) 
                 (spriteTexture spr) 
                 (ry) -- dist
  | otherwise = Nothing 
          
  where 
    (mx',my') = spritePos spr `vecSub` viewPos 
    (mx,my)   = (fromIntegral mx',fromIntegral my')
    rx      = fromIntegral$ floori_$ mx * cos (-viewAngle) - my * sin (-viewAngle) 
    ry      = fromIntegral$ floori_$ my * cos (-viewAngle) + mx * sin (-viewAngle) 
    
    dist    = sqrt (rx*rx+ry*ry)
    
    mw = fromIntegral $ floori_ (256*(fromIntegral (vcViewDistance vc)/ dist))
    mh = fromIntegral $ floori_ (256*(fromIntegral (vcViewDistance vc)/ dist))
    projx = rx * fromIntegral (vcViewDistance vc) / ry  
                
    projx_ = (fromIntegral (floori_ projx)) + (viewportCenterX vc - (mw `div` 2))    
    
-- An objected projected onto screen 
data RItem = RItem { rItemPos  :: (Int32,Int32), -- position on screen
                     rItemDims :: (Int32,Int32),
                     rItemTexture :: Surface, 
                     rItemDepth   :: Float} -- distance from Viewer (used for clipping against walls) 
                     
                       
renderRItem :: Surface -> [Float] -> RItem -> IO () 
renderRItem surf dists ritem = 
  drawTransparentZ (rItemTexture ritem) 
                   surf 
                   (Rect x y w h) dist dists
  where 
    x = fromIntegral$ fst $ rItemPos ritem                 
    y = fromIntegral$ snd $ rItemPos ritem 
    w = fromIntegral$ fst $ rItemDims ritem
    h = fromIntegral$ snd $ rItemDims ritem
    dist = rItemDepth ritem 


drawTransparentZ :: Surface -> Surface -> Rect -> Float -> [Float] -> IO ()                
drawTransparentZ  tr surf (Rect x y w h) depth depths 
  | outside = return () -- sprite is completely outside of target surface  
  | otherwise = 
    do 
      seeThrough <- mapRGB pf 255 0 255 
      targPixels <- castPtr `fmap` surfaceGetPixels surf
      srcPixels  <- castPtr `fmap` surfaceGetPixels tr 
      
      -- No visible improvement in speed.
      let depthsArr = listArray (0,length depths-1) depths        
                  
      sequence_ [do 
                  pixel <- peekElemOff srcPixels 
                                       (fromIntegral (floori_ (xJump+(fromIntegral i*rx)))+
                                        (fromIntegral columns)* 
                                        fromIntegral (floori_ (yJump+(fromIntegral j *ry))))
                                       
                  -- how bad is it to use a depths list (lookups are linear                      
                  -- but there are only a maximum of viewportWidth lookups per frame.
                  -- Probably bad anyway
                  if ((Pixel pixel) /= seeThrough && depth < (depthsArr ! (clippedX+i)))  
                  then pokeElemOff targPixels (start+(i+width*j)) (pixel :: Word32) 
                  else return ()
                  -- if (depth > (depthsArr ! (clippedX+i))) 
                  -- then putStrLn "clipping dist"
                  -- else return ()

                | i <- [0..clippedW-1] , j <- [0..clippedH-1]] 
                  
    where 
      
      rx      = (fromIntegral columns / fromIntegral w) 
      ry      = (fromIntegral rows / fromIntegral h) 


      -- if completely outside.  
      outside = (x > width || y > height || 
                 x < -w || y < -h)
 
      clippedX = x1' 
      clippedY = y1' 
 
      xJump    = rx * fromIntegral (clippedX - x) --how far to jump in texture
      yJump    = ry * fromIntegral (clippedY - y)

      (x1',y1') = (if x < 0 then 0 else x, if y < 0 then 0 else y) 
      (x2',y2') = (if (x+w) >= width then width-1 else x+w, 
                   if (y+h) >= height then height-1 else y+h) 

      clippedW = x2'-x1'
      clippedH = y2'-y1'
      
      start   = clippedX + clippedY * width 
      width   = surfaceGetWidth surf
      height  = surfaceGetHeight surf
     
      pf      = surfaceGetPixelFormat surf
      columns = surfaceGetWidth tr  
      rows    = surfaceGetHeight tr  



{-     
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
    
-} 
----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  
    
  let vc = mkViewConfig (floori_ (800.0*0.6))   
                        800 
                        600 
                        (256,256) 
    
  setVideoMode 800 600 32 []

  
  screen <- getVideoSurface
  -- toggleFullscreen screen
  
  putStrLn$ arr2dStr$ testLevelArr
  
  let pf = surfaceGetPixelFormat screen
      
  --testTexture' <- loadBMP "Data/textureLarge1.bmp" 
  --testTexture <- convertSurface testTexture' pf [] 
  --floorTex'   <- loadBMP "Data/floor1.bmp"            
  --floorTex    <- convertSurface floorTex' pf [] 
                 
  wallTextures <- sequence [conv pf =<< loadBMP "Data/textureLarge1.bmp"
                           ,conv pf =<< loadBMP "Data/textureLarge2.bmp"
                           ,conv pf =<< loadBMP "Data/textureLarge1.bmp"]
                 
  -- These textures are not in the repo yet.          
  --wallTextures <- sequence [conv pf =<< loadBMP "Data/Wall2.bmp"
  --                         ,conv pf =<< loadBMP "Data/Wall3.bmp"
  --                         ,conv pf =<< loadBMP "Data/Door1.bmp"]
  
  
  floorTextures <- sequence [conv pf =<< loadBMP "Data/floor1.bmp"
                            ,conv pf =<< loadBMP "Data/floor2.bmp"
                            ,conv pf =<< loadBMP "Data/floor3.bmp"
                            ,conv pf =<< loadBMP "Data/floor4.bmp" ]

  
  monster <- conv pf =<< loadBMP "Data/eye1.bmp"  
  let monsterSprite = Sprite (256+128,13*256+128)
                             0
                             (256,256) 
                             monster 
                   
  
                 
  eventLoop vc screen floorTextures wallTextures -- testTexture floorTex
    monsterSprite
    (False,False,False,False) -- Keyboard state
    (0.0,256+128 ,256+128)
  
  quit
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: ViewConfig 
             -> Surface 
             -> [Surface] 
             -> [Surface] 
             -> Sprite
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Int32, Int32) 
             -> IO ()
eventLoop vc screen floorTextures wallTextures monster (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
  
  -- draw all the visible walls
  -- floorCast  testLevelFloorArr x y r screen floorTextures
  
  fillRect screen 
           (Just (Rect 0 0 800 600)) 
           =<< mapRGB pf 2 2 2 
  
  slices <- renderWalls vc testLevelArr ((x,y),r) wallTextures screen
  -- newFloorCast testLevelFloorArr (x,y) r slices floorTextures screen
  
  let dists  = map sliceDistance slices 
  
  -- testSprite. 
                             
  let monsterTfrmd = viewTransformSprite vc ((x,y),r) monster
  maybe (return ()) (renderRItem screen dists) monsterTfrmd
  
  
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

  unless b $ eventLoop vc screen floorTextures wallTextures monster (up',down',left',right') (r',x',y')     
  
  
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
    movementAllowed (px,py) = testLevelArr !! (px `div` wallWidth vc,py `div` wallWidth vc) == 0
