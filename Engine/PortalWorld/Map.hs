
{- 2012 Joel Svensson -} 


module Engine.PortalWorld.Map where 

import Data.Int 

import Engine.Math

---------------------------------------------------------------------------- 
-- Map


data MapType = MapType {mapWalls :: [Wall]} 
            

data Wall = Portal Line Vector2D MapType 
            -- Portal into another "area".
            -- The vector is pointing into the area it leads to. 
          | Wall Line Int32 
            -- The int ids the texture to use for this wall. 
          
            
mkWall :: Point2D -> Point2D -> Int32 -> Wall             
mkWall p1 p2 ident = Wall (Line p1 p2) ident 

mkPortal :: Point2D -> Point2D -> Vector2D -> MapType -> Wall 
mkPortal p1 p2 v area = 
  Portal (Line p1 p2) v area
  
----------------------------------------------------------------------------