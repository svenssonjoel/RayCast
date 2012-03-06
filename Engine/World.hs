{- 2012 Joel Svensson -} 


module Engine.World where

import Graphics.UI.SDL

import Engine.Math
import Engine.Light
import Engine.Slice 
import Engine.ViewConfig

import Data.Int



---------------------------------------------------------------------------- 
--  Changed interface! 
-- Having a class for this seems a bit pointless..

class World a where 
  renderView :: ViewConfig -> a -> Lights -> View -> [Surface] -> Surface -> IO [Slice] 
  -- castRay :: ViewConfig -> a -> Lights -> View -> Int32 -> IO Slice 