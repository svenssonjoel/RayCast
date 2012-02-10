
{- 
  RItem. render item

  2012 Joel Svensson 
-} 


module Engine.RItem where 

import Graphics.UI.SDL

import Engine.Math
import Engine.RayCast
import Engine.ZBuffer

--import Foreign.Ptr
--import Foreign.Storable
--import Foreign.Marshal.Array

import Data.Int
import Data.Word
import Data.Array
import Data.Bits
import Data.List 

import CExtras
import MathExtras

-- An objected projected onto screen 
data RItem = RItem { rItemPos  :: Point2D, -- position on screen
                     rItemWorldPos :: Point2D,  
                     rItemDims :: (Int32,Int32),
                     rItemTexture :: Surface, 
                     rItemDepth   :: Float,
                     rItemIntensityR :: Float,
                     rItemIntensityG :: Float,
                     rItemIntensityB :: Float} -- distance from Viewer (used for clipping against walls) 
                     
sortRItems = sortBy depth 
  where 
    depth r1 r2 = compare (rItemDepth r2) (rItemDepth r1) 

                                   
renderRItem :: Surface -> ZBuffer -> RItem -> IO () 
renderRItem surf dists ritem = 
  renderRItemC_ x y w h surf (rItemTexture ritem) 
                             (rItemIntensityR ritem) 
                             (rItemIntensityG ritem)
                             (rItemIntensityB ritem)
                             dist dists
  where 
    x = fst $ rItemPos ritem                 
    y = snd $ rItemPos ritem 
    w = fst $ rItemDims ritem
    h = snd $ rItemDims ritem
    dist = rItemDepth ritem 


-- DONE: Implement this function in C  (only problem is what to do with the depths list) 
{- 
drawTransparentZ :: Surface -> Surface -> Rect -> Float -> [Float] -> IO ()                
drawTransparentZ  tr surf (Rect x y w h) depth depths 
  | outside = return () -- sprite is completely outside of target surface  
  | otherwise = 
    do 
      -- seeThrough <- mapRGB pf 255 0 255 
      targPixels <- castPtr `fmap` surfaceGetPixels surf
      srcPixels  <- castPtr `fmap` surfaceGetPixels tr 
      
      -- No visible improvement in speed.
      let depthsArr = listArray (0,length depths-1) depths        
                  
      sequence_ [do 
                  (p :: Word32) <- peekElemOff srcPixels 
                                       (fromIntegral (floori_ (xJump+(fromIntegral i*rx)))+
                                        (fromIntegral columns)* 
                                        fromIntegral (floori_ (yJump+(fromIntegral j *ry))))
                                       
                  let intensity = (min 1.0 (32768/(depth*depth))) 
                  let p0  = p .&. 255 
                      p1  = p `shiftR` 8 .&. 255 
                      p2  = p `shiftR` 16 .&. 255 
                      p3  = p `shiftR` 24 .&. 255 -- Alfa (seethroughness) 
                      
                      p0' =  floor_ $ intensity * (fromIntegral p0) 
                      p1' =  floor_ $ intensity * (fromIntegral p1) 
                      p2' =  floor_ $ intensity * (fromIntegral p2) 
                                
                      p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
                  
                  -- how bad is it to use a depths list (lookups are linear                      
                  -- but there are only a maximum of viewportWidth lookups per frame.
                  -- Probably bad anyway
                  if (p3 /= 0 {-(Pixel p) /= seeThrough-} && depth < (depthsArr ! (clippedX+i)))  
                  then pokeElemOff targPixels (start+(i+width*j)) (p' :: Word32) 
                  else return ()
                  
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
     
      -- pf      = surfaceGetPixelFormat surf
      columns = surfaceGetWidth tr  
      rows    = surfaceGetHeight tr  
-} 