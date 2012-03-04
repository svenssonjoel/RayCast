{-# LANGUAGE FlexibleContexts #-}

{- 2012 Joel Svensson -} 

module Engine.CubeWorld.Map where 

-- import Data.Array
import Data.Int 

import Data.Array.Storable
import Data.Array.MArray

import Engine.Math
----------------------------------------------------------------------------
-- Maptype ... expand upon 

data MapType = MapType Int32 Int32 (StorableArray Int32 Int32)
-- type MapType = Array2D Int32 Int32
               
listMap :: (MArray StorableArray Int32 m, Monad m) 
           => (Int32,Int32) 
           -> [[Int32]] 
           -> m MapType
listMap (w,h) dat = 
  do 
    dat' <- newListArray (0,h*w) (concat dat) 
    return$ MapType w h dat' -- (listArray (0,(h*w)) (concat dat))


(!!) (MapType width _ arr) (x,y) = readArray arr (y*width+x) 

arr2dStr (MapType width height arr) 
  = "array" -- unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 

mapWidth (MapType w _ _) = w
mapHeight (MapType _ h _) = h 

mapGrid (MapType w h _) ww = 
  [yline ww y| y <- [0..h]] ++  
  [xline ww x| x <- [0..w]]
  where 
    xline w x = 
      Line (mkPoint (fromIntegral (x * ww),0))
           (mkPoint (fromIntegral (x * ww),8192))
    
    yline w y = 
      Line (mkPoint (0,fromIntegral (y * ww))) 
           (mkPoint (8192,fromIntegral (y * ww)))        
    
                              
