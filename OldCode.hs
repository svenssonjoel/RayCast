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


----------------------------------------------------------------------------      
-- Cast for floors 
{- 
newFloorCast :: ViewConfig 
                -> MapType 
                -> [Light]
                -> [Int32]
                -> View 
                -> [Surface] 
                -> Surface 
                -> IO ()
newFloorCast vc world lights slices view textures surf = 
  do 
    
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures

    mapM_ (lerpRow_ vc world lights slices tps surf)  edges
               
  where 
    edges = [
             (y+viewportCenterY vc,(newFloorCastPoint vc world view x1 y,
                 newFloorCastPoint vc world view x2 y))
            | y <- [0..(viewportCenterY vc-1)]] 
    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
    
lerpRow_ :: ViewConfig -> MapType -> [Light] -> [Int32] -> [(Pixels,Int32)] -> Surface -> (Int32, ((Float,Float),(Float,Float))) -> IO ()     
lerpRow_ vc world lights slices tps surf (y,(p1,p2)) = 
         lerpRowC (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps)
                  surf
                  lights                  
                  (fromIntegral (length lights))
                  y
                  (fst p1) (snd p1)
                  (fst p2) (snd p2)
             
newFloorCast2 :: ViewConfig 
                 -> MapType 
                 -> [Light]
                 -> [Int32]
                 -> View 
                 -> [Surface] 
                 -> Surface 
                 -> IO ()
newFloorCast2 vc world lights slices view textures surf = 
  do 
    
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures
    lerpRows_ vc world lights slices tps surf x1s y1s x2s y2s 
               
  where 
    -- TODO: Storable points. 
    -- TODO: Storable Pairs of points. (lines)  
    (x1s,y1s) = unzip edgeL
    (x2s,y2s) = unzip edgeR
    
    edgeL = [newFloorCastPoint vc world view x1 y
            | y <- [0..(viewportCenterY vc-1)]] 
    edgeR = [newFloorCastPoint vc world view x2 y
            | y <- [0..(viewportCenterY vc-1)]] 
    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
    
-} 
{-
newFloorCast3 :: ViewConfig 
                 -> MapType 
                 -> Lights 
                 -> [Int32]
                 -> View 
                 -> [Surface] 
                 -> Surface 
                 -> IO ()
newFloorCast3 vc world lights slices view textures surf = 
  do 
    tps <- mapM (\x -> do e <- castPtr `fmap` surfaceGetPixels x; return (e,fromIntegral (surfaceGetWidth x))) textures
    lerpRows_2 vc world lights slices tps surf rl 
               
  where                 
    rl =  [mkRealLine (newFloorCastPoint vc world view x1 y)                 
                      (newFloorCastPoint vc world view x2 y) 
           | y <- [0..(viewportCenterY vc - 1)]]
                    
    x1 = 0;
    x2 = vcWindowWidth vc - 1 
-} 

{- 
lerpRows_ :: ViewConfig 
             -> MapType 
             -> [Light] 
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> IO ()     
lerpRows_ vc world lights slices tps surf p1x p1y p2x p2y = 
         lerpRowsC (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) -- (castPtr tps') 
                  surf
                  lights                  
                  (fromIntegral (length lights))
                  p1x p1y
                  p2x p2y

-} 
{-
lerpRows_2 :: ViewConfig 
             -> MapType 
             -> Lights
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> [Float] 
             -> IO ()     
lerpRows_2 vc world lights slices tps surf p1x p1y p2x p2y = 
         lerpRowsC_ (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) 
                  surf
                  (lightsPtr lights)
                  (lightsNum lights) -- (fromIntegral (length lights))
                  p1x p1y
                  p2x p2y
-} 
{-
lerpRows_2 :: ViewConfig 
             -> MapType 
             -> Lights
             -> [Int32] 
             -> [(Pixels,Int32)] 
             -> Surface 
             -> [RealLine] 
             -> IO ()     
lerpRows_2 vc world lights slices tps surf rl = 
         lerpRowsC_  vc -- (wallWidth vc) (modMask vc) (vcWindowWidth vc)
                  16 16 world 
                  slices 
                  (map fst tps) 
                  surf
                  (lightsPtr lights)
                  (lightsNum lights) -- (fromIntegral (length lights))
                  rl



newFloorCastPoint :: ViewConfig -> MapType -> View -> Int32 -> Int32 -> (Float,Float)    
newFloorCastPoint vc world (pos,angle) x y = 
  ps
  where 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (x {-column-} - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    ps = ((point2DGetX pos) - distance * sin radians
         ,(point2DGetY pos) + distance * cos radians)
    
    distance = rowDistance y 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral row 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle

-} 
{-     
lerpRow :: ViewConfig -> MapType -> [Light] -> Array Int Int32 -> Array Int (Ptr CInt,Int32) -> Surface -> (Int32, ((Float,Float),(Float,Float))) -> IO () 
lerpRow vc world lights slices tps surf (y,(p1,p2)) = 
  do 
    sp <- castPtr `fmap` surfaceGetPixels surf
    sequence_ [do 
                  let (inx,iny) = ((floori_ (fromIntegral xi * rX+(fst p1))),
                                   (floori_ (fromIntegral xi * rY+(snd p1)))) 
                      (inR,inG,inB) = clamp 1.0 $ foldl1 vec3add (map (lightContribution (inx,iny))  lights)
                      (wx,wy) = ((inx `div` wallWidth vc) .&. 15, 
                                 (iny `div` wallWidth vc) .&. 15) 
                      (tx,ty) = (inx .&. modMask vc,
                                 iny .&. modMask vc)
                      
                  -- texture mapping    
                  tix  <- fmap fromIntegral (world !! (wx,wy))
                  let (tp,w) = (tps  ! tix ) 
                      t      = tx + ty * (fromIntegral w) -- fromIntegral (surfaceGetWidth tex)   
                 
                  
                
                  if (slices ! (fromIntegral xi) <= y)     
                     -- speeds up quite a bit when not much floor is visible
                    then 
                     do 
                      texPointC tx ty w tp xi y width sp inR inG inB
                      texPointC tx ty w tp xi (vcWindowHeight vc - y) width sp inR inG inB
                    else 
                      return () 
                  {- 
                  (p :: Word32) <- peekElemOff tp (fromIntegral t)  
                  let p0  = p .&. 255 
                      p1  = p `shiftR` 8 .&. 255 
                      p2  = p `shiftR` 16 .&. 255 
                      -- p3  = p `shiftR` 24 .&. 255 
                      p0' =  floor_ $ inB * (fromIntegral p0) 
                      p1' =  floor_ $ inG * (fromIntegral p1) 
                      p2' =  floor_ $ inR * (fromIntegral p2) 
                                
                      p'  = p0' -- + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
                  pokeElemOff sp (fromIntegral (xi+y*width)) p'     -- floor... 
                  -}
              | xi <- [0..width-1]]
    
  where 
    
    rX = (fst p2 - fst p1) / fromIntegral width
    rY = (snd p2 - snd p1) / fromIntegral width
    width = vcWindowWidth vc
    x1 = 0; 
    

   -} 



-- The slices are just there to be able to make some optimisations.              
{-     
floorCast :: ViewConfig -> MapType -> [Light] -> Point2D -> Angle -> [Slice] -> [Surface] -> Surface -> IO ()              
floorCast vc world lights pos angle slices textures surf =              
  zipWithM_ (floorCastColumn vc world lights pos angle textures surf) slices [0..vcWindowWidth vc -1]
             
floorCastColumn :: ViewConfig -> MapType -> [Light] -> Point2D -> Float -> [Surface] -> Surface -> Slice -> Int32 -> IO ()
floorCastColumn vc world lights (px,py) angle tex surf slice col = 
  do 
    pixels <- castPtr `fmap` surfaceGetPixels surf 
    texels <- mapM ((return . castPtr) <=< surfaceGetPixels) tex

    sequence_ [renderPoint texels pixels r col xyd  
               | (r,xyd)<- zip rows ps]  
  where 
    -- compute light 
    radians = angle - columnAngle
    columnAngle = atan (fromIntegral (col - viewportCenterX vc) / fromIntegral (vcViewDistance vc))
    
    -- only check floor where there are no walls (optimisation) 
    rows = [sliceBot slice..vcWindowHeight vc-1]
    -- rows = [viewportCenterY+1..windowHeight-1]              


    ps = [(fromIntegral px - distance * sin radians 
          ,fromIntegral py + distance * cos radians,distance )
         | r <- rows
         , let distance = rowDistance r] 
    
    ratioHeightRow row = 128 {-fromIntegral viewerHeight-} / fromIntegral (row - viewportCenterY vc) 
                         
    
    rowDistance row = ratioHeightRow row * fromIntegral (vcViewDistance vc) / cos columnAngle
         
    renderPoint :: [Ptr Word32] -> Ptr Word32 -> Int32 -> Int32 -> (Float,Float,Float) -> IO ()      
    renderPoint tex surf row col (x,y,dist) = 
      do 
        let (tx,ty) = (floori_ x `div` wallWidth vc, floori_ y `div` wallWidth vc) 
        
        index <- fmap fromIntegral (world !! (tx,ty))
        p  <- peekElemOff (tex P.!! index)  (fromIntegral t) 
        
        let (inR,inG,inB) = clamp 1.0 $ foldl vec3add (0,0,0) (map (lightContribution (floori_ x,
                                                                                       floori_ y))  lights)
   
        -- let i = (min 1.0 (32768/(dist*dist))) 
        let p0  = p .&. 255 
            p1  = p `shiftR` 8 .&. 255 
            p2  = p `shiftR` 16 .&. 255 
            -- p3  = p `shiftR` 24 .&. 255 
            p0' =  floor_ $ inB * (fromIntegral p0) 
            p1' =  floor_ $ inG * (fromIntegral p1) 
            p2' =  floor_ $ inR * (fromIntegral p2) 
                                
            p'  = p0' + (p1' `shiftL` 8) + (p2' `shiftL` 16)  -- + (p3' `shiftL` 24)
        
        pokeElemOff surf (fromIntegral r)  p'     -- floor... 
        pokeElemOff surf (fromIntegral r2) p'     -- ceiling...   
        
        where 
          t  = ((floori_ y .&. modMask vc) * textureWidth + (floori_ x .&. modMask vc))
          r  = (row * (vcWindowWidth vc) + col)
          r2 = ((vcWindowHeight vc -row-1) * (vcWindowWidth vc) + col )
          textureWidth = 256 -- fix this !!! surfaceGetWidth tex
  
-} 
