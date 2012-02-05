{-# LANGUAGE FlexibleContexts #-}

module Engine.Map where 

-- import Data.Array
import Data.Int 


import Data.Array.Storable
import Data.Array.MArray
----------------------------------------------------------------------------
-- Maptype ... expand upon 

data Array2D i e = Array2D Int32 (StorableArray i e)
type MapType = Array2D Int32 Int32
               
listMap :: (MArray StorableArray Int32 m, Monad m) 
           => (Int32,Int32) 
           -> [[Int32]] 
           -> m MapType
listMap (w,h) dat = 
  do 
    dat' <- newListArray (0,h*w) (concat dat) 
    return$ Array2D w dat' -- (listArray (0,(h*w)) (concat dat))


(!!) (Array2D width arr) (x,y) = readArray arr (y*width+x) 

arr2dStr (Array2D width arr) 
  = "array" -- unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 


