{-
   2012 Joel Svensson 
-} 

module Engine.ZBuffer (ZBuffer(..),
                       withZBuffer) where 

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array 

data ZBuffer = ZBuffer {fromZBuffer :: Ptr CFloat}


withZBuffer :: [Float] -> (ZBuffer -> IO a)  -> IO a
withZBuffer dists f = withArray (map realToFrac dists) (\z -> f (ZBuffer z))


