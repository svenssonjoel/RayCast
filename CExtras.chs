{-# LANGUAGE ForeignFunctionInterface #-}

module CExtras where 

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable hiding (sizeOf)
import Foreign.Marshal.Array
import Data.Word
import Data.Int

import Graphics.UI.SDL

import Engine.ZBuffer 

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
    realToFrac   `Float',
    realToFrac   `Float', 
    realToFrac   `Float'  } -> `()' id  #}

--{# fun unsafe renderRItem as renderRItemC 
-- { fromIntegral `Int' ,
--   fromIntegral `Int' ,
--   fromIntegral `Int' ,
--   fromIntegral `Int' ,  -- the Rect
--   convSurface* `Surface' , 
--   convSurface* `Surface' , 
--   realToFrac   `Float' ,
--   withFloatArray* `[Float]' } -> `()' id #}

{# fun unsafe renderRItem as renderRItemC_ 
 { fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,
   fromIntegral `Int' ,  -- the Rect
   convSurface* `Surface' , 
   convSurface* `Surface' ,
   realToFrac   `Float' ,
   realToFrac   `Float' ,
   realToFrac   `Float' , 
   realToFrac   `Float' ,
   fromZBuffer  `ZBuffer' } -> `()' id #}


-- void texPoint(int tx, int ty, int tw, int32_t *text,
--              int x, int y, int w, int32_t *surf, 
--	      float inR, float inG, float inB){

{# fun unsafe texPoint as texPointC 
   { fromIntegral `Int32' , 
     fromIntegral `Int32' , 
     fromIntegral `Int32' , 
     id           `Ptr CInt' ,
     fromIntegral `Int32' , 
     fromIntegral `Int32' , 
     fromIntegral `Int32' ,
     id           `Ptr CInt' ,
     realToFrac   `Float' , 
     realToFrac   `Float' , 
     realToFrac   `Float' } -> `()' id #}