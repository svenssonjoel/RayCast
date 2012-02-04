

module Engine.Map where 

import Data.Array
import Data.Int 

----------------------------------------------------------------------------
-- Maptype ... expand upon 

data Array2D i e = Array2D Int32 (Array i e)
type MapType = Array2D Int32 Int32
               

listMap (w,h) dat = Array2D w (listArray (0,(h*w)) (concat dat))

(!!) (Array2D width arr) (x,y) = arr ! (y*width+x) 

arr2dStr (Array2D width arr) 
  = "array" -- unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 


