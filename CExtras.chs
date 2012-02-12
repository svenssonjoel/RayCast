{-# LANGUAGE ForeignFunctionInterface #-}

module CExtras where 

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable -- ing (sizeOf)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Array.Storable
import Data.Word
import Data.Int

import System.IO.Unsafe
import Control.Monad
import Control.Applicative

import Graphics.UI.SDL hiding (with)


-- Divide cExtras into smaller modules ?
import Engine.ZBuffer 
import Engine.Map 
import Engine.Math

#include "cExtras.h" 

type Pixels = Ptr Int32

data Light = Light !Int32 !Int32 
                   !Float !Float !Float 

instance Storable Light where 
  sizeOf _ = {#sizeof light #} 

  alignment a = 4 -- ? 
  peek p = 
    do  
      px <- fromIntegral `fmap` ({#get light->lx #} p) 
      py <- fromIntegral `fmap` ({#get light->ly #} p) 
      r <- realToFrac `fmap` ({#get light->inR #} p) 
      g <- realToFrac `fmap` ({#get light->inG #} p) 
      b <- realToFrac `fmap` ({#get light->inB #} p) 
      return$ Light px py r g b 

  poke p (Light px py r g b) = do 
    {#set light.lx #} p (fromIntegral $ px) 
    {#set light.ly #} p (fromIntegral $ py) 
    {#set light.inR #} p (realToFrac $ r) 
    {#set light.inG #} p (realToFrac $ g) 
    {#set light.inB #} p (realToFrac $ b) 

convSurface s f = do 
  withForeignPtr  s $ \ptr -> (f (castPtr ptr))
  
convLight l f = do 
  withArray l $ \ptr -> (f (castPtr ptr)) 
  
withPixels ps f = 
  withArray ps $ \ptr -> f (castPtr ptr)
                                                  
-- why do I needs all these "castPtr"s everywhere ?
withPoint p f = with p $ \ptr -> f (castPtr ptr)
withDims  p f = with p $ \ptr -> f (castPtr ptr)
                          
withMap (MapType w arr) f = 
  withStorableArray arr $ \ptr -> f (castPtr ptr)
  
withIntArray xs = withArray (fmap fromIntegral xs)  
withFloatArray xs = withArray (fmap realToFrac xs)
 
peekFloat ptr = realToFrac `fmap`  peek ptr                    

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
 { withPoint* `Point2D' ,
   withDims*  `Dims2D' ,
   convSurface* `Surface' , 
   convSurface* `Surface' ,
   realToFrac   `Float' ,
   fromZBuffer  `ZBuffer', 
   withPoint*   `Point2D' ,
   castPtr      `Ptr Light' ,
   fromIntegral `Int32' } -> `()' id #}

{-{# fun unsafe renderRItem as renderRItemC_ 
 { fromIntegral `Int32' ,
   fromIntegral `Int32' ,
   fromIntegral `Int32' ,
   fromIntegral `Int32' ,  -- the Rect
   convSurface* `Surface' , 
   convSurface* `Surface' ,
   realToFrac   `Float' ,
   fromZBuffer  `ZBuffer', 
   fromIntegral `Int32' , 
   fromIntegral `Int32' ,
   castPtr      `Ptr Light' ,
   fromIntegral `Int32' } -> `()' id #}
-}



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

{-
void lerpRow(int32_t wallWidth, 
	     int32_t modMask,
	     int32_t windowWidth, // things from ViewConfig that matters to this fun
	     
	     int32_t mapW, 
	     int32_t mapH, 
	     int32_t *map,
	     
	     int32_t *bots,
             int32_t **textures,
	     SDL_Surface *surf,
	     
	     int32_t y,
	     float   p1x, float p1y, 
	     float   p2x, float p2y) 
-}
{# fun unsafe lerpRow as lerpRowC 
  { fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    withMap*     `MapType' , 
    withIntArray* `[Int32]' , 
    withPixels*  `[Pixels]', 
    convSurface* `Surface' ,
    convLight*   `[Light]' ,
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    realToFrac   `Float' ,
    realToFrac   `Float' ,
    realToFrac   `Float' ,
    realToFrac   `Float' } -> `()' id #}


{# fun unsafe lerpRows as lerpRowsC 
  { fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    withMap*     `MapType' , 
    withIntArray* `[Int32]' , 
    withPixels*  `[Pixels]', 
    convSurface* `Surface' ,
    convLight*   `[Light]' ,
    fromIntegral `Int32' , 
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' } -> `()' id #}


{# fun unsafe lerpRows as lerpRowsC_ 
  { fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    fromIntegral `Int32' , 
    withMap*     `MapType' , 
    withIntArray* `[Int32]' , 
    withPixels*  `[Pixels]', 
    convSurface* `Surface' ,
    castPtr      `Ptr Light' ,
    fromIntegral `Int32' , 
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' ,
    withFloatArray*   `[Float]' } -> `()' id #}
    
    
    

{# fun unsafe computeLight as computeLight
   { alloca- `Float' peekFloat* , 
     alloca- `Float' peekFloat* , 
     alloca- `Float' peekFloat* , 
     fromIntegral `Int32' , 
     fromIntegral `Int32' , 
     castPtr      `Ptr Light' ,
     fromIntegral `Int32' } -> `()' id #}
     
     