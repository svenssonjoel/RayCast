{-2012 Joel Svensson -}

module Engine.PortalWorld.RayCast where

import Engine.Math
import Engine.Light 
import Engine.RGB
import Engine.Slice 
import Engine.ViewConfig
import Engine.World

import Engine.PortalWorld.Map

instance World MapType where 
  castRay = undefined 