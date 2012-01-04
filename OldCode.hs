----------------------------------------------------------------------------
-- CastRay is a quite direct translation from the C code in the Book
-- TODO: horizontal_crossing_at_x  etc are really bad names (they are wrong) 
{- 
castRay :: Array2D Int Int -> Int -> Int -> Int -> Int -> Float -> (Float,Int)
castRay world x y cx cy colAngle = if (xp < 0 || xp > 15 || yp < 0 || yp > 15) 
                                      then error "You are outside the world!" -- (100.0,1)  -- just make something up if value > 0 
                                      else 
                                        if (value > 0) 
                                        then (distance',value)  
                                        else castRay world x y nx ny colAngle  
                                   
  where 
    xdiff = floor (1024*(cos colAngle))
    ydiff = floor (1024*(sin colAngle))
    grid_x = if (xdiff > 0) 
             then (cx .&. 0xffc0) + 64
             else (cx .&. 0xffc0) - 1
    grid_y = if (ydiff > 0) 
             then (cy .&. 0xffc0) + 64
             else (cy .&. 0xffc0) -1 
    horizontal_crossing_at_x = (fromIntegral grid_x) 
    horizontal_crossing_at_y = (fromIntegral cy) + (slope xdiff ydiff) * (fromIntegral (grid_x - cx))  
    vertical_crossing_at_x   = (fromIntegral cx) + (fromIntegral (grid_y-cy)) / (slope xdiff ydiff)
    vertical_crossing_at_y   = (fromIntegral grid_y)
    horizontal_dist = dist (horizontal_crossing_at_x - (fromIntegral x), 
                            horizontal_crossing_at_y - (fromIntegral y)) 
    vertical_dist   = dist (vertical_crossing_at_x - (fromIntegral x),                  
                            vertical_crossing_at_y - (fromIntegral y))
    (xp,yp,nx,ny)  = if (horizontal_dist < vertical_dist) 
                     then (floor (horizontal_crossing_at_x / 64), 
                           floor (horizontal_crossing_at_y / 64),
                           floor horizontal_crossing_at_x,
                           floor horizontal_crossing_at_y) 
                     else (floor (vertical_crossing_at_x / 64),      
                           floor (vertical_crossing_at_y / 64),
                           floor vertical_crossing_at_x,
                           floor vertical_crossing_at_y) 
    distance' = min horizontal_dist vertical_dist 
    value = world !! (xp,yp) 
    
slope :: (Integral a, Fractional b, Ord b)  => a -> a -> b 
slope dx dy = if sl == 0.0 then 0.0001 else sl   
  where 
    sl = ((fromIntegral dy) / (fromIntegral dx))

dist (xd,yd) = max 1 (sqrt (xd*xd+yd*yd))
-}




-- Event handling
  {-
  let (r',x',y',b) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> (r-0.1,x,y,False)
              SDLK_RIGHT -> (r+0.1,x,y,False)
              SDLK_UP    -> 
                let 
                    dx = 32 * cos r
                    dy = 32 * sin r
                in (r,x+dx,y+dy,False)
              SDLK_DOWN  -> 
                let 
                    dx = 32 * cos r
                    dy = 32 * sin r
                in (r,x-dx,y-dy,False)
                
                
              otherwise  -> (r,x,y,False)
          Quit -> (r,x,y,True) -- quit 
          otherwise -> (r,x,y,False)
  -} 