

module Engine.Map where 

import Data.Array 
import Data.Int 


----------------------------------------------------------------------------
-- Maptype ... expand upon 

type Array2D i e = Array i (Array i e) 
type MapType = Array2D Int32 Int32

(!!) arr (x,y) = (arr ! y) ! x 
arr2dStr arr = unlines (map concat [[show ((arr ! y) ! x)| x <- [0..15]]| y <- [0..15]]) 
