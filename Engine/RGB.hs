{- 2012 Joel Svensson -} 
module Engine.RGB where 

import Engine.Math

data RGB = RGB Vector3Df

mkRGB r g b = RGB $ mkVector3Df r g b

