{- 2012 Joel Svensson -}

module Engine.Sprite where 

import Graphics.UI.SDL

import Engine.RayCast 
import Engine.Math
import Engine.RItem

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
viewTransformSprite :: ViewConfig -> [Light] -> View -> Sprite -> Maybe RItem
viewTransformSprite vc lights (viewPos,viewAngle) spr  
  | ry > 0 = --(using > in place of >= fixed a visible glitch) 
    Just $ RItem (projx_,viewportCenterY vc -(mh `div` 2)) 
                 (mw,mh) 
                 (spriteTexture spr) 
                 (ry) -- dist
                 inR inG inB 
  | otherwise = Nothing 
          
  where 
    (mx',my') = spritePos spr `vecSub` viewPos 
    (mx,my)   = (fromIntegral mx',fromIntegral my')
    rx      = fromIntegral$ floori_$ mx * cos (-viewAngle) - my * sin (-viewAngle) 
    ry      = fromIntegral$ floori_$ my * cos (-viewAngle) + mx * sin (-viewAngle) 
    
    
    -- Compute light 
    (inR,inG,inB) = clamp 1.0 $ foldl vec3add (0,0,0) (map (lightContribution (spritePos spr))  lights)
  
    
    
    dist    = sqrt (rx*rx+ry*ry)
    
    mw = fromIntegral $ floori_ (256*(fromIntegral (vcViewDistance vc)/ dist))
    mh = fromIntegral $ floori_ (256*(fromIntegral (vcViewDistance vc)/ dist))
    projx = rx * fromIntegral (vcViewDistance vc) / ry  
                
    projx_ = (fromIntegral (floori_ projx)) + (viewportCenterX vc - (mw `div` 2))    