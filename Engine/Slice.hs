{- 2012 Joel Svensson -} 

module Engine.Slice (Slice(..))
                     where 

import Data.Int 

data Slice = Slice {sliceTop :: Int32,
                    sliceBot :: Int32, 
                    sliceTex :: Int32,
                    sliceTexCol :: Int32,
                    sliceIntensityR :: Float,  -- USE RGB !
                    sliceIntensityG :: Float, 
                    sliceIntensityB :: Float,
                    sliceDistance :: Float}