{- 2012 Joel Svensson -} 

module Engine.ViewConfig 
       (mkViewConfig, 
        viewportCenterX, 
        viewportCenterY,
        wallWidth, 
        wallHeight, 
        gridMask, 
        modMask, 
        module CExtras )  where

import CExtras (ViewConfig(..)) 

----------------------------------------------------------------------------
-- ViewConfiguration

  
mkViewConfig = ViewConfig 

viewportCenterX = (`div` 2) . vcWindowWidth 
viewportCenterY = (`div` 2) . vcWindowHeight
                  
wallWidth  = fst . vcWallDims 
wallHeight = snd . vcWallDims 

gridMask = negate . modMask  
modMask  vc = wallWidth vc - 1  

