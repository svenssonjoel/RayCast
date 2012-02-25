{- 2012 Joel Svensson -} 
module Engine.RGB where 


import Engine.Math

import Foreign.Storable
import Foreign.Ptr


----------------------------------------------------------------------------
type RGB = Vector3Df

mkRGB r g b = mkVector3Df r g b
