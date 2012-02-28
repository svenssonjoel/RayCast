{- 2012 Joel Svensson -} 
module Engine.RGB where 

import Engine.Math

----------------------------------------------------------------------------
type RGB = Vector3Df

mkRGB r g b = mkVector3Df r g b
