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

{-   
floorCast :: Array2D Int32 Int32 -> Int32 -> Int32 -> Float -> Surface -> [Surface] -> IO ()              
floorCast world px py angle surf texture = 
    sequence_ [floorCastColumn world px py angle surf texture col
               | col <- [0..windowWidth-1]]
      
-- Approach to try next is to draw line by line  
-- This draws column by column
--  + a Hack to draw ceilings as well. 
-- TODO: Right now this completely ignores the map passed in
floorCastColumn :: Array2D Int32 Int32 -> Int32 -> Int32 -> Float -> Surface -> [Surface] -> Int32 -> IO ()
floorCastColumn world px py angle surf tex col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- mapM (\s -> do ptr <- surfaceGetPixels s; return (castPtr ptr)) tex
       
    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    radians = angle + columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX) / fromIntegral viewDistance)
    rows = [viewportCenterY+1..windowHeight-1]              


    ps = [(fromIntegral px - distance * sin radians 
          ,fromIntegral py + distance * cos radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
    
    ratioHeightRow row = fromIntegral viewerHeight / fromIntegral (row - viewportCenterY) 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral viewDistance / cos columnAngle
         
    renderPoint :: [Ptr Word32] -> Ptr Word32 -> Int32 -> Int32 -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        -- Read one Word32 instead of 4 word8
        -- why does this produce visibly ok results with "mod 16" ?? 
        -- The mod 16 kicks in when floor outside of the map is being "cast"
        let (tx,ty) = (floori_ x `div` wallWidth `mod` 16, floori_ y `div` wallWidth `mod` 16) 
        
        
        
        p  <- peekElemOff (tex P.!! (fromIntegral (world !! (tx,ty)))) (fromIntegral t) 
        
        let i = (min 1.0 (lightRadius/dist)) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ i * (fromIntegral p0) 
            p1' =  floor_ $ i * (fromIntegral p1) 
            p2' =  floor_ $ i * (fromIntegral p2) 
                                
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf (fromIntegral r)  p'     -- floor... 
        pokeElemOff surf (fromIntegral r2) p'     -- ceiling...   
        
        where 
          t  = ((floori_ y .&. modMask) * textureWidth + (floori_ x .&. modMask))
          r  = (row * windowWidth + col)
          r2 = ((windowHeight-row) * windowWidth + col )
    
-} 