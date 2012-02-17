{- 2012 Joel Svensson -}

module Engine.Sprite where 

import Graphics.UI.SDL

import Engine.RayCast 
import Engine.Math
import Engine.RItem
import Engine.ScreenPoint

import MathExtras
import CExtras

----------------------------------------------------------------------------
-- Sprites 
data Sprite = Sprite { spritePos       :: Point2D,      -- world x,y pos 
                       spriteElevation :: Float,        -- height above ground (z) 
                       spriteDims      :: (Float,Float),-- Base size  
                       spriteTexture   :: Surface}
                       

----------------------------------------------------------------------------
-- World space object to screen space renderable object 
viewTransformSprite :: ViewConfig -> View -> Sprite -> Maybe RItem
viewTransformSprite vc (viewPos,viewAngle) spr  
  | ry > 0 = --(using > in place of >= fixed a visible glitch) 
    Just $ RItem (mkScreenPoint (floor projx_,
                                 floor ((fromIntegral (viewportCenterY vc) - (mh / 2)))))
                 (spritePos spr)
                 (mkScreenDims (floor mw,
                                floor mh)) 
                 (spriteTexture spr) 
                 ry -- dist
                 -- inR inG inB 
  | otherwise = Nothing 
          
  where 

    (Point2D mx my) = spritePos spr - viewPos 
    rx      = mx * cos (-viewAngle) - my * sin (-viewAngle) 
    ry      = my * cos (-viewAngle) + mx * sin (-viewAngle) 
    
    
    dist    = sqrt (rx*rx+ry*ry)
    (ow,oh) = spriteDims spr
    mw = ow * (fromIntegral (vcViewDistance vc) / dist)
    mh = oh * (fromIntegral (vcViewDistance vc) / dist)
    projx = rx * fromIntegral (vcViewDistance vc) / ry  
                
    projx_ = projx + (fromIntegral (viewportCenterX vc) - (mw / 2))