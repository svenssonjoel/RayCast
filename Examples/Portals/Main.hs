{-
    
   Raycasting in a world with portals.
   
   2012 Joel Svensson  
-} 

module Main where 

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as FONT

import Control.Monad
import qualified Control.Monad.State.Strict as S 

import Data.Word
import Data.Int
import Data.List hiding (intersect)
import Data.Maybe
import Data.Array

import Engine.Math
import Engine.Render
import Engine.RItem
import Engine.Sprite 
import Engine.ZBuffer
import Engine.Light 
import Engine.ViewConfig
import Engine.Slice
import Engine.World
import Engine.PortalWorld.Map 
import Engine.PortalWorld.RayCast


import CExtras
import MathExtras

----------------------------------------------------------------------------
-- test 

testWorld1 = MapType [mkWall (mkPoint (-512,-512)) (mkPoint (-512,-128)) 1, 
                      mkWall (mkPoint (-512,-128)) (mkPoint (-640, 0))   1, 
                      mkWall (mkPoint (-640, 0  )) (mkPoint (-512, 128)) 1,  
                      mkWall (mkPoint (-512, 128)) (mkPoint (-512, 512)) 1,  
                      
                      mkWall (mkPoint (-512, 512)) (mkPoint ( 512, 512)) 2,
                      mkWall (mkPoint ( 512, 512)) (mkPoint ( 512,-512)) 3, 
                      mkWall (mkPoint ( 512,-512)) (mkPoint (-512,-512)) 4]

walkSpeed      = 32

----------------------------------------------------------------------------
-- TODO
    
data FPSCalc = FPSCalc {fpsCalcTicks  :: Word32,  
                        fpsCalcFrames :: Int32, 
                        fpsCalcFPS    :: Float} 
    
data ArrowKeys = ArrowKeys {arrowKeyUp    :: Bool,                
                            arrowKeyDown  :: Bool,  
                            arrowKeyLeft  :: Bool, 
                            arrowKeyRight :: Bool} 
               
data GameState = GameState {gsTarget :: Surface,
                            gsWallTextures :: [Surface], 
                            gsSprite :: Sprite, 
                            gsWorld  :: MapType,
                            gsFont   :: Font, 
                            gsFPS    :: FPSCalc, 
                            gsKeyState :: ArrowKeys, 
                            gsViewAngle :: Float,
                            gsViewPos   :: Point2D}
                                                       
type GS a = S.StateT GameState IO a           

gsGetTarget :: GS Surface 
gsGetTarget = S.gets gsTarget  

gsGetWallTextures :: GS [Surface] 
gsGetWallTextures = S.gets gsWallTextures 

gsGetWorld :: GS MapType
gsGetWorld = S.gets gsWorld

gsUpdateWorld :: (MapType -> MapType) -> GS MapType
gsUpdateWorld f = 
  do 
    gs <- S.get 
    let w = gsWorld gs
        w' = f w 
    S.put $ gs { gsWorld = w' } 
    return w'
  

gsGetSprite :: GS Sprite
gsGetSprite = S.gets gsSprite  

gsGetFont :: GS Font
gsGetFont   = S.gets gsFont 

gsGetFPS :: GS Float
gsGetFPS    = do 
  fpsCalc <- S.gets gsFPS 
  return $ fpsCalcFPS fpsCalc
  
gsUpdateFPS :: (FPSCalc -> FPSCalc) -> GS () 
gsUpdateFPS f = S.modify (\gs -> gs {gsFPS = (f . gsFPS) gs})
  
  
gsUpdateArrowKeys :: (ArrowKeys -> ArrowKeys) -> GS ArrowKeys
gsUpdateArrowKeys f = 
  do 
    gs <- S.get 
    let arrowKeys = gsKeyState gs
        arrowKeys' = f arrowKeys 
    S.put $ gs { gsKeyState = arrowKeys' } 
    return arrowKeys'
    
    
gsUpdateArrowKeys' e = gsUpdateArrowKeys (\ars -> f ars )      
   
  where  
    f (ArrowKeys up down left right) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT    -> ArrowKeys up down True right
              SDLK_RIGHT   -> ArrowKeys up down left True 
              SDLK_UP      -> ArrowKeys True down left right 
              SDLK_DOWN    -> ArrowKeys up True left right 
              SDLK_ESCAPE  -> ArrowKeys up down left right 
              otherwise    -> ArrowKeys up down left right 
          (KeyUp k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> ArrowKeys up down False right 
              SDLK_RIGHT -> ArrowKeys up down left False 
              SDLK_UP    -> ArrowKeys False down left right 
              SDLK_DOWN  -> ArrowKeys up False left right 
              otherwise  -> ArrowKeys up down left right 
            -- Quit -> (up,down,left,right) 
          otherwise -> ArrowKeys up down left right 
    
gsUpdateViewAngle :: (Float -> Float) -> GS Float 
gsUpdateViewAngle f = 
  do 
    gs <- S.get 
    let r = gsViewAngle gs
        r' = f r 
    S.put $ gs { gsViewAngle = r' } 
    return r'
                          
gsUpdateViewPos :: (Point2D -> Point2D) -> GS Point2D 
gsUpdateViewPos f =  
  do
    gs <- S.get 
    let p = gsViewPos gs
        p' = f p
    S.put $ gs { gsViewPos = p' } 
    return p'
   
  
  

----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  FONT.init  
    
  fnt <- openFont "../../Data/LiberationMono-Regular.ttf" 14 
    
  let vc = mkViewConfig (floori_ (800.0*0.6)) -- viewDist       
                        128 -- viewHeight
                        800 -- windowWidth
                        600 -- windowHeight
                        (256,256) -- meaningless in this case. 
                        
         
         
  setVideoMode (fromIntegral (vcWindowWidth vc)) 
               (fromIntegral (vcWindowHeight vc)) 32 []

  
  screen <- getVideoSurface
  -- toggleFullscreen screen
  
  -- something is very wrong with the mousemotion events 
  enableEvent SDLMouseMotion False 
  
  
  let pf = surfaceGetPixelFormat screen
  
  wallTextures <- sequence [conv pf =<< loadBMP "../../Data/textureLarge1.bmp"
                           ,conv pf =<< loadBMP "../../Data/textureLarge2.bmp"]
                 
  
  monster <- conv pf =<< loadBMP "../../Data/eye1.bmp"  
  let monsterSprite = Sprite (mkPoint (0,0))
                             0
                             (256,256) 
                             monster 
   

  initialTicks <- getTicks
  S.evalStateT  (eventLoop vc) $ GameState screen 
                                           wallTextures 
                                           monsterSprite 
                                           testWorld1 
                                           fnt
                                           (FPSCalc initialTicks 0 0.0)
                                           (ArrowKeys False False False False) -- Keyboard state
                                           0.0
                                           (mkPoint (128 ,128))
                                       
  FONT.quit
  SDL.quit
  
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: ViewConfig -> GS ()   
eventLoop vc = do 
  pf <- surfaceGetPixelFormat `fmap` gsGetTarget 
  
  pix <- S.lift $ mapRGB pf 16 16 16 
  
  -- Improve on this 
  screen <- gsGetTarget
  currWorld <- gsGetWorld
  wallTextures <- gsGetWallTextures
  p@(Point2D x y)  <- gsUpdateViewPos id 
  r     <- gsUpdateViewAngle id 
  monster <- gsGetSprite 
  fnt <- gsGetFont
  
  -- Clear screen
  S.lift $ fillRect screen 
                    (Just (Rect 0 0 (fromIntegral (vcWindowWidth vc)) (fromIntegral (vcWindowHeight vc)))) 
                    pix
  
  let lights = [mkLight (x,y) (1.0,1.0,1.0),
                mkLight (-256,-256) (1.0,0.0,0.0), 
                mkLight ( 512, 512) (0.0,1.0,0.0)]
  
  -- draw all walls
  S.lift$  withLights lights $ \ lights' -> 
    do 
      slices <- renderWalls vc currWorld lights' (p,r) wallTextures screen
  
      let dists  = map sliceDistance slices 
  
      -- testSprite. 
                             
      let monsterTfrmd = viewTransformSprite vc (p,r) monster
      withZBuffer dists $ \zbuf -> 
        maybe (return ()) (renderRItem screen zbuf lights' ) monsterTfrmd
 
  S.lift$ SDL.flip screen
  
  -- process events 
  e <- S.lift pollEvent
  
  -- Handle arrowkey presses
  ArrowKeys  up' down' left' right' <- gsUpdateArrowKeys' e 
  
  -- Handle the escape key
  let b = case e of 
            (KeyDown k) -> symKey k == SDLK_ESCAPE 
            Quit        -> True
            otherwise -> False
   
  let (r',x',y') = (moveLeft left' . moveRight right' . moveUp up' . moveDown down') (r,x,y)         
          
  let portals       = filter isPortal (mapWalls currWorld)
  let portIntersect = filter ((/= Nothing) . fst) [(intersect (Ray (mkPoint (x,y)) (mkVector (x'-x,y'-y))) l,p) | p@(Portal l _ _) <-  portals]  
  let currWorld'    = 
        case (map snd portIntersect) of 
          [] -> currWorld
          [(Portal _ _ world')] -> world'
          _ -> error "what!"
      
  -- FIX FIX FIX     
  gsUpdateViewPos (\_ -> (mkPoint (x',y'))) 
  gsUpdateViewAngle (\_ -> r') 
  gsUpdateArrowKeys (\_ -> (ArrowKeys up' down' left' right'))
  gsUpdateWorld (\_ -> currWorld')     
      
  -- if not exit, loop.    
  unless b $ eventLoop vc 
  
  where 
    
    moveLeft  b (r,x,y) = if b then (r+0.08,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r-0.08,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (fromIntegral walkSpeed*sin r)
        y' = y + (fromIntegral walkSpeed*cos r)
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (fromIntegral walkSpeed*sin r)
        y' = y - (fromIntegral walkSpeed*cos r)
    movementAllowed (px,py) = True -- Just allow it. for now.

    
    isPortal (Portal _ _ _) = True
    isPortal _ = False
    
    
-- Causes segfault     
renderMsg fnt message (x,y) surf 
  = do 
    txt <- renderTextSolid fnt message (Color 255 255 255)  
    blitSurface txt Nothing surf (Just (Rect x y 800 600))
    freeSurface txt
  
    
    