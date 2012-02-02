{-# LANGUAGE ForeignFunctionInterface #-}

module CExtras where 

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable hiding (sizeOf)
import Foreign.Marshal.Array

import Graphics.UI.SDL

#include "cExtras.h" 


convSurface s f = do 
  withForeignPtr  s $ \ptr -> (f (castPtr ptr))
  
withIntArray xs = withArray (fmap fromIntegral xs)  
withFloatArray xs = withArray (fmap realToFrac xs)
 
{# fun unsafe texturedVLine as texVLine 
  { fromIntegral `Int' , 
    fromIntegral `Int' , 
    fromIntegral `Int' ,
    convSurface* `Surface', 
    fromIntegral `Int',
    fromIntegral `Int',
    fromIntegral `Int',
    convSurface* `Surface' } -> `()' id  #}

{# fun unsafe texturedVLineLit as texVLineLit 
  { fromIntegral `Int' , 
    fromIntegral `Int' , 
    fromIntegral `Int' ,
    convSurface* `Surface', 
    fromIntegral `Int',
    fromIntegral `Int',
    fromIntegral `Int',
    convSurface* `Surface',
    realToFrac   `Float'  } -> `()' id  #}

{# fun unsafe renderRItem as renderRItemC 
 { fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,  -- the Rect
   convSurface* `Surface' , 
   convSurface* `Surface' , 
   realToFrac   `Float' ,
   withFloatArray* `[Float]' } -> `()' id #}

{# fun unsafe renderRItem as renderRItemC_ 
 { fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,  -- the Rect
   convSurface* `Surface' , 
   convSurface* `Surface' , 
   realToFrac   `Float' ,
   id           `Ptr CFloat' } -> `()' id #}



-- void renderRItem(int x, int y, int w, int h, SDL_Surface *surf, // Target rect and surface 
--                  SDL_Surface *text, float depth, float *depths) { // sprite image and depth and world depths 
  