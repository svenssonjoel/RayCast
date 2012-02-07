{-# LANGUAGE ForeignFunctionInterface #-}

module CExtras where 

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable -- ing (sizeOf)
import Foreign.Marshal.Array
import Data.Array.Storable
import Data.Word
import Data.Int

import Control.Monad
import Control.Applicative

import Graphics.UI.SDL


-- Divide cExtras into smaller modules ?
import Engine.ZBuffer 
import Engine.Map 

#include "cExtras.h" 

type Pixels = Ptr Int32

mkLight (x,y) (r,g,b) = Light x y r g b 
data Light = Light {
               lx'light :: Int32,
               ly'light :: Int32 , 
               inR'light :: Float,
               inG'light :: Float,
               inB'light :: Float 
              } 

instance Storable Light where 
  sizeOf _ = {#sizeof light #} 

  alignment a = 4
  peek p = Light <$>  liftM fromIntegral ({#get light->lx #} p)
                 <*>  liftM fromIntegral ({#get light->ly #} p)
                 <*>  liftM realToFrac   ({#get light->inR #} p) 
                 <*>  liftM realToFrac   ({#get light->inR #} p) 
                 <*>  liftM realToFrac   ({#get light->inR #} p)
  poke p x = do 
    {#set light.lx #} p (fromIntegral $ lx'light x) 
    {#set light.ly #} p (fromIntegral $ ly'light x) 
    {#set light.inR #} p (realToFrac $ inR'light x) 
    {#set light.inG #} p (realToFrac $ inG'light x) 
    {#set light.inB #} p (realToFrac $ inB'light x) 

  --peekByteOff :: Ptr b -> Int -> IO a
{-   peekElemOff ptl offs = 
    do
      (x :: Int32) <- peekElemOff (castPtr ptl) offs  
      (y :: Int32) <- peekElemOff (castPtr ptl) (offs + sizeOf x) 
      (r :: Float) <- peekElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y)
      (g :: Float) <- peekElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y + sizeOf r)
      (b :: Float) <- peekElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y + sizeOf r + sizeOf g) 
      
      return (Light (x,y) (r,g,b)) 
  -- pokeElemOff :: Ptr a -> Int -> a -> IO ()     
  pokeElemOff ptl offs (Light (x,y) (r,g,b)) = 
    do 
      pokeElemOff (castPtr ptl) offs x 
      pokeElemOff (castPtr ptl) (offs + sizeOf x) y
      pokeElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y) r
      pokeElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y + sizeOf r) g 
      pokeElemOff (castPtr ptl) (offs + sizeOf x + sizeOf y + sizeOf r + sizeOf g) b
-} 
{- 
ata StructName = StructName
  { struct_field1'StructName :: Int
  , struct_field2'StructName :: Int
  }
instance Storable StructName where
  sizeOf _ = {#sizeof StructName #}
  alignment _ = 4
  peek p = StructName
    <$> liftM cIntConv ({#get StructName->struct_field1 #} p)
    <*> liftM cIntConv ({#get StructName->struct_field2 #} p)
  poke p x = do
    {#set StructName.struct_field1 #} p (cIntConv $ struct_field1'StructName x)
    {#set StructName.struct_field2 #} p (cIntConv $ struct_field2'StructName x)

-} 

convSurface s f = do 
  withForeignPtr  s $ \ptr -> (f (castPtr ptr))
  
convLight l f = do 
  withArray l $ \ptr -> (f (castPtr ptr)) 
  
withPixels ps f = 
  withArray ps $ \ptr -> (f (castPtr ptr))
                          
withMap (MapType w arr) f = 
  withStorableArray arr $ \ptr -> f (castPtr ptr)
  
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
    
    
    