{-# LANGUAGE FlexibleContexts #-}

{- 2012 Joel Svensson -} 

module Engine.CubeWorld.Map where 

-- import Data.Array

import Graphics.UI.SDL

import Data.Int 

import Data.Array.Storable
import Data.Array.MArray

import Engine.Math
----------------------------------------------------------------------------
-- Maptype ... expand upon 

data MapType = MapType  {mapWidth   :: Int32, 
                         mapHeight  :: Int32,  
                         mapWalls   :: (StorableArray Int32 Int32), 
                         mapFloor   :: (StorableArray Int32 Int32), 
                         mapCeiling :: (StorableArray Int32 Int32),
                         mapWallTextures :: [Surface], 
                         mapFloorTextures :: [Surface], 
                         mapCeilingTextures :: [Surface]}
                                   
-- type MapType = Array2D Int32 Int32
               
{-                
listMap :: (MArray StorableArray Int32 m, Monad m) 
           => (Int32,Int32) 
           -> [[Int32]] 
           -> m MapType
listMap (w,h) dat = 
  do 
    dat' <- newListArray (0,h*w) (concat dat) 
    return$ MapType w h dat' -- (listArray (0,(h*w)) (concat dat))
-} 

mkMap :: (MArray StorableArray Int32 m, Monad m) 
         => (Int32,Int32) 
         -> [[Int32]] -- walls 
         -> [[Int32]] -- floor
         -> [[Int32]] -- ceiling
         -> [Surface] 
         -> [Surface] 
         -> [Surface] 
         -> m MapType
mkMap (w,h) walls floor ceil wt ft ct =          
  do 
    w' <- newListArray (0,h*w) (concat walls)
    f' <- newListArray (0,h*w) (concat floor)
    c' <- newListArray (0,h*w) (concat ceil)
    return$ MapType w h w' f' c' wt ft ct 


(!!) (MapType width _ walls _ _ _ _ _) (x,y) = readArray walls (y*width+x) 

arr2dStr (MapType width height walls floor ceil wt ft ct) 
  = "array" 

-- Generate a grid to intersect rays against 
mapGrid (MapType w h _ _ _ _ _ _) ww = 
  [yline ww y| y <- [0..h]] ++  
  [xline ww x| x <- [0..w]]
  where 
    xline w x = 
      Line (mkPoint (fromIntegral (x * ww),0))
           (mkPoint (fromIntegral (x * ww),8192))
    
    yline w y = 
      Line (mkPoint (0,fromIntegral (y * ww))) 
           (mkPoint (8192,fromIntegral (y * ww)))        
    
                              

