module Engine.Light where 

import Data.Int
import Foreign.Ptr
import Foreign.Marshal.Array

import CExtras -- where Light is defined 



data Lights = Lights {lightsNum :: Int32, 
                      lightsPtr :: Ptr Light}   
              
              
withLights ls f = 
  withArray ls $ \ptr -> f (Lights (fromIntegral (length ls)) ptr)               
              
