
module Engine.ScreenCoords where 

import Foreign.C.Types
import Foreign.Storable

import Data.Int

----------------------------------------------------------------------------

data ScreenPoint = ScreenPoint Int32 Int32 
                   deriving (Eq,Show)
                            
mkScreenPoint (x,y) = ScreenPoint x y 
                            
instance Storable ScreenPoint where 
  sizeOf _ = sizeOf (undefined :: Int32) * 2
  alignment _ = 4
  peek p = do 
    x <- peekByteOff p 0 :: IO Int32
    y <- peekByteOff p s :: IO Int32 
    return $ ScreenPoint x y 
    where s = sizeOf (undefined :: Int32)
  poke p (ScreenPoint x y) = do 
    pokeByteOff p 0 x    
    pokeByteOff p s y 
    where s = sizeOf (undefined :: Int32)



data ScreenDims = ScreenDims Int32 Int32 
                deriving (Eq,Show) 
                         
mkScreenDims (w,h) = ScreenDims w h 
                         
instance Storable ScreenDims where 
  sizeOf _ = sizeOf (undefined :: Int32) * 2
  alignment _ = 4
  peek p = do 
    w <- peekByteOff p 0 :: IO Int32
    h <- peekByteOff p s :: IO Int32
    return $ ScreenDims w h 
    where s = sizeOf (undefined :: Int32)
  poke p (ScreenDims w h) = do 
    pokeByteOff p 0 w
    pokeByteOff p s h 
    where s = sizeOf (undefined :: Int32) 
                         