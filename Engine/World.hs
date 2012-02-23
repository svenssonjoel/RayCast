{- 2012 Joel Svensson -} 


module Engine.World where

import Engine.Math
import Engine.Light
import Engine.Slice 
import Engine.ViewConfig

import Data.Int



---------------------------------------------------------------------------- 
-- Intersect a ray against a world. 
-- Potentially light the slice given a set of Lights  

class World a where 
  castRay :: ViewConfig -> a -> Lights -> View -> Int32 -> IO Slice 