
module SDLUtils where 

import Graphics.UI.SDL 

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Word

----------------------------------------------------------------------------
-- 
putPixel x y (Pixel pixel) surf =  
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    pokeElemOff pixels ((y * surfaceGetWidth surf) + x) pixel

putPixelRGB x y (r,g,b) surf = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf
    let pf = surfaceGetPixelFormat surf 
    pixel  <- mapRGB pf r g b 
    pokeElemOff pixels ((y * surfaceGetWidth surf) + x) pixel
       
-- Does not work, no stretch-blit in sdl 
texturedColumn x y1 y2 tex xt surf =       
    blitSurface tex fromRect surf toRect
  where 
    fromRect = Just (Rect xt 0 1 32)    
    toRect   = Just (Rect x y1 1 (y2 - y1))
    


vertLine x y1 y2 (r,g,b) surf = 
  do 
    let pf = surfaceGetPixelFormat surf 
    pixel  <- mapRGB pf r g b
    pixels <- castPtr `fmap` surfaceGetPixels surf
    let w     = surfaceGetWidth surf 
        start = y1 * w + x
        
    sequence_ [pokeElemOff pixels (start + (i*w))  pixel 
              | i <- [0..(y2-y1)]
              ] 
    

texturedVLine x y1 y2 surf xt yt1 yt2 tex = 
  do 
    let pf = surfaceGetPixelFormat surf 
    tex' <- convertSurface tex pf [] 
    texPix  <- castPtr `fmap` surfaceGetPixels tex'
    surfPix <- castPtr `fmap` surfaceGetPixels surf 
    sequence_ [ do
                   p <- peekElemOff texPix (tstart + ((floor((fromIntegral i)*ratio))*64))
                   pokeElemOff surfPix (start + (i*w)) (p :: Word32) 
      
              | i <- [0..lineHeight-1]
              ]
    
  where 
    start  = y1 * w + x 
    tstart = yt1 * 64 + xt  
    w          = surfaceGetWidth surf 
    lineHeight = y2 - y1 
    texHeight  = yt2 - yt1 
    ratio      = (fromIntegral texHeight) / (fromIntegral lineHeight)
    
    
