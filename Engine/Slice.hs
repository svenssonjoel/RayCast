{- 2012 Joel Svensson -} 

module Engine.Slice (Slice(..))
                     where 

import Engine.RGB
import Data.Int 

import CExtras

data Slice = Slice {sliceTop :: Int32,
                    sliceBot :: Int32, 
                    sliceTex :: Int32,
                    sliceTexCol :: Int32,
                    sliceIntensity :: RGB, 
                    sliceDistance :: Float}
             
             


