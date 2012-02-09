/* 2012 Joel Svensson 
   
   
 */ 

#include <stdint.h>
#include <math.h>

#include <SDL/SDL.h> 

#include <stdio.h>
#include "cExtras.h"
/* 
   TODO: move computation of light intensity into texturedVLine. 
   TODO:   the same for renderRItem. 
 */

/* -----------------------------------------------------------------------------


   -------------------------------------------------------------------------- */
void texturedVLine(int x, int y0, int y1, SDL_Surface *surf,
		   int xt, int yt0, int yt1, SDL_Surface *text) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
  // int th = text->h;
  int clipped_y1 = y0 > 0 ? y0 : 0;
  int clipped_y2 = y1 < sh ? y1 : sh;
 
  int lineHeight = y1 - y0; 
  int texHeight = yt1 - yt0; 

  unsigned int *sp =(unsigned int*)surf->pixels;
  unsigned int *tp =(unsigned int*)text->pixels;  
  
  float ratio = (float)texHeight / lineHeight;
    
  for(y = clipped_y1; y<clipped_y2; y++){
    
    int ty = (y - y0) * ratio;
    
    sp[y * sw + x] = tp[texHeight * ty + xt];
  } 
}


/* -----------------------------------------------------------------------------


   -------------------------------------------------------------------------- */
void texturedVLineLit(int x, int y0, int y1, SDL_Surface *surf,
		      int xt, int yt0, int yt1, SDL_Surface *text, 
		      float intensityR, 
		      float intensityG,
		      float intensityB) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
 
  int clipped_y1 = y0 > 0 ? y0 : 0;
  int clipped_y2 = y1 < sh ? y1 : sh;
 
  int lineHeight = y1 - y0; 
  int texHeight = yt1 - yt0; 

  int32_t *sp =(int32_t*)surf->pixels;
  int32_t *tp =(int32_t*)text->pixels;  

  float ratio = (float)texHeight / lineHeight;
  
  for(y = clipped_y1; y<clipped_y2; y++){
    
    float ty = (y - y0) * ratio;
    int32_t p = 0;
    int32_t t = tp[texHeight*(int)ty+xt];
    unsigned char *p_ = (unsigned char*)&p;
    unsigned char *t_ = (unsigned char*)&t; 

    p_[0] = intensityB * t_[0];
    p_[1] = intensityG * t_[1]; 
    p_[2] = intensityR * t_[2];

    sp[y*sw+x] = p; 
  } 
}

/* -----------------------------------------------------------------------------
   Render a textured billboarded 2D sprite  (called RItem in the Haskell code) 
   -------------------------------------------------------------------------- */

void renderRItem(int x, int y, int w, int h, SDL_Surface *surf, // Target rect and surface 
                 SDL_Surface *text, 
                 float inR, 
		 float inG, 
		 float inB, 
                 float depth, float *depths) { // sprite image and depth and world depths 
  
  int width   = surf->w;
  int height  = surf->h;
  int columns = text->w;
  int rows    = text->h;

  // if completely outside of target, just skip.
  if (x > width || y > height || x < -w || y < -h) return; 
  
  int32_t *targPixels = (int32_t*)surf->pixels;
  int32_t *srcPixels  = (int32_t*)text->pixels;
  
  int x1 = x < 0 ? 0 : x;
  int y1 = y < 0 ? 0 : y;
 
  int x2 = (x+w) >= width  ? width-1  : x+w;
  int y2 = (y+h) >= height ? height-1 : y+h; 
  

  int clippedW = x2-x1;
  int clippedH = y2-y1;
  // if (!clippedW || !clippedH) return; // should not matter (very rare!) 

  int start = x1 + y1 * width;

  float rx = (float)columns / w;
  float ry = (float)rows / h; 

  int xJump = (rx * (float)(x1-x));
  int yJump = (ry * (float)(y1-y)); 

  int j;
  int i; 
 
  //  float intensity = fmin(1.0,32768.0/(depth*depth));


  for (j = 0; j < clippedH; j++) { 
    for (i = 0; i < clippedW; i++) {
      int32_t p = srcPixels[(xJump+(int)(i*rx))+
			    columns * (yJump + (int)(j*ry))];

      if (depth >= depths[x1+i]) continue; 

      unsigned char *p_ = (unsigned char*)&p;
      
      if (p_[3] != 0) { 	
	p_[0] *= inB;
	p_[1] *= inG;
	p_[2] *= inR; 
       
	// write the now updated p to target. 
	targPixels[start+(i+width*j)] = p;
      }
     
    }

  }
  
}


void texPoint(int tx, int ty, int tw, int32_t *text,
              int x, int y, int w, int32_t *surf, 
	      float inR, float inG, float inB){

  int32_t p = text[tx+ty*tw];
  unsigned char *p_ = (unsigned char*)&p; 
  p_[0] *= inB;
  p_[1] *= inG;
  p_[2] *= inR; 
  surf[x+y*w] = p; 

}

//void texturedVLineLit(int x, int y0, int y1, SDL_Surface *surf,
//		      int xt, int yt0, int yt1, SDL_Surface *text, 
//		      float intensityR, 
//		      float intensityG,
//		      float intensityB) {

void lerpRow(int32_t wallWidth, 
	     int32_t modMask,
	     int32_t windowWidth, // things from ViewConfig that matters to this fun
	     
	     int32_t mapW, 
	     int32_t mapH, 
	     int32_t *map,
	     
	     int32_t *bots,
             int32_t **textures,
	     SDL_Surface *surf,
	     light *lights, 
	     int32_t num_lights,
	     
	     int32_t y,
	     float   p1x, float p1y, 
	     float   p2x, float p2y) {


  int32_t *sp = (int32_t*)surf->pixels;

  float rx = (float)(p2x - p1x) / (float)windowWidth;
  float ry = (float)(p2y - p1y) / (float)windowWidth; 
  int xi;

  for (xi = 0; xi < windowWidth; ++xi) { 
    if (bots[xi] > y) continue;
    int32_t inx = (int32_t)(xi * rx + p1x);
    int32_t iny = (int32_t)(xi * ry + p1y);
    
    int32_t wx = (inx / wallWidth) & 15; // something other than 15 might occur!! 
    int32_t wy = (iny / wallWidth) & 15;
    
    int32_t tx = inx & modMask;
    int32_t ty = iny & modMask;
    
    int32_t  ti = map[wx  + wy * 16]; 
    int32_t* tp = textures[ti];
    
    float inR = 0.0;
    float inG = 0.0;
    float inB = 0.0; 
    int l;
    for (l = 0; l < num_lights; l++) {
      
      float xd = lights[l].lx - inx;
      float yd = lights[l].ly - iny;
      double dist = sqrt (xd*xd + yd*yd) / 256;
      double ld = 1 / fmax(0.01,(dist*dist));
      inR += fmin(1.0, lights[l].inR * ld); 
      inG += fmin(1.0, lights[l].inG * ld); 
      inB += fmin(1.0, lights[l].inB * ld); 
	
    } 
    inR = fmin(1.0,inR);
    inG = fmin(1.0,inG);
    inB = fmin(1.0,inB);
    
    
    texPoint(tx,ty,256,tp,
	     xi,y,windowWidth,sp,
	     inR,inG,inB);
    texPoint(tx,ty,256,tp,
	     xi,600-y,windowWidth,sp,
	     inR,inG,inB);
  }
      


}
	     

void lerpRows(int32_t wallWidth, 
	      int32_t modMask,
	      int32_t windowWidth, // things from ViewConfig that matters to this fun
	     
	      int32_t mapW, 
	      int32_t mapH, 
	      int32_t *map,
	     
	      int32_t *bots,
              int32_t **textures,
	      SDL_Surface *surf,
	      light *lights, 
	      int32_t num_lights,
	     
	      float   *p1xa, float *p1ya, 
	      float   *p2xa, float *p2ya) {


  int32_t *sp = (int32_t*)surf->pixels;
  int yi; //  = 300; 
  for (yi = 0; yi < 300; ++yi){
    int y = yi + 300;
    
    // precalc some 
    int yww = y*windowWidth; 
    int yww_ = (599-y)*windowWidth;

    // the line being interpolated
    float p1x = p1xa[yi];
    float p1y = p1ya[yi];
    float p2x = p2xa[yi]; 
    float p2y = p2ya[yi];

    float inR = 0.0;
    float inG = 0.0;
    float inB = 0.0; 
    
    // hardcoded 1/800
    float rx = (float)(p2x - p1x) * 1.25e-3; // / (float)windowWidth;
    float ry = (float)(p2y - p1y) * 1.25e-3; // / (float)windowWidth; 
    int xi;
  
    for (xi = 0; xi < windowWidth; ++xi) { 
      if (bots[xi] > y) continue;
      int32_t inx = (int32_t)(xi * rx + p1x);
      int32_t iny = (int32_t)(xi * ry + p1y);
    
      int32_t wx = (inx / wallWidth) & 15; // something other than 15 might occur!! 
      int32_t wy = (iny / wallWidth) & 15;
    
      int32_t tx = inx & modMask;
      int32_t ty = iny & modMask;
    
      int32_t  ti = map[wx + (wy << 4)]; // wy * 16 
      int32_t* tp = textures[ti];

      
      inR = inG = inB = 0.0;
      int l;
      for (l = 0; l < num_lights; l++) {
	float xd = lights[l].lx - inx;
	float yd = lights[l].ly - iny;
	double dist = sqrt (xd*xd + yd*yd);//  / 256;
	double ld = 65536 / (dist*dist); // fmax(0.01,(dist*dist));
	inR += lights[l].inR * ld; 
	inG += lights[l].inG * ld; 
	inB += lights[l].inB * ld; 
      } 
      inR = fmin(1.0,inR);
      inG = fmin(1.0,inG);
      inB = fmin(1.0,inB);
    
      
      // inlined texPoint
      int32_t p = tp[tx+(ty<<8)]; // ty * 256
      unsigned char *p_ = (unsigned char*)&p; 
      p_[0] *= inB;
      p_[1] *= inG;
      p_[2] *= inR; 
      sp[xi+yww] = p;
      sp[xi+yww_] = p;
    }
  }    
}



	     

/* 
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
*/

/* Textured vertical line that borrows ideas from http://lodev.org/cgtutor/raycasting.html */
/*   
void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
  int th = text->h;
  int clipped_y1 = y1 > 0 ? y1 : 0;
  int clipped_y2 = y2 < sh ? y2 : sh;
 
  int lineHeight = y2 - y1; 
  int texHeight = yt2 - yt1; 
  if (texHeight == 0) texHeight = 1;

  unsigned int *sp =(unsigned int*)surf->pixels;
  unsigned int *tp =(unsigned int*)text->pixels;  
  
  int lh128 = lineHeight*128;
  int lh256 = lh128 << 1; 
  int sh128 = sh*128;
  int shlhdiff = sh128 - lh128;

  for(y = clipped_y1; y<clipped_y2; y++){
    int d = y * 256 - shlhdiff;  
    int texY = (d * texHeight) / lh256;
      
     
      sp[y * sw + x] = tp[texHeight * texY + xt];
    } 
}


void texturedVLineLit(int x, int y1, int y2, SDL_Surface *surf,
		      int xt, int yt1, int yt2, SDL_Surface *text, float intensity) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
  int th = text->h;
  int clipped_y1 = y1 > 0 ? y1 : 0;
  int clipped_y2 = y2 < sh ? y2 : sh;
 
  int lineHeight = y2 - y1; 
  int texHeight = yt2 - yt1; 
  // if (texHeight == 0) texHeight = 1;

  unsigned char *sp =(unsigned char*)surf->pixels;
  unsigned char *tp =(unsigned char*)text->pixels;  

  int lh128 = lineHeight*128;
  int lh256 = lh128 << 1; 
  int sh128 = sh*128;
  int shlhdiff = sh128 - lh128;

  for(y = clipped_y1; y<clipped_y2; y++){
    int d = y * 256 - shlhdiff;  
    int texY = (d * texHeight) / lh256;
    
    
    sp[4 *(y * sw + x)]   = intensity * tp[4*(texHeight * texY + xt)];
    sp[4 *(y * sw + x)+1] = intensity * tp[4*(texHeight * texY + xt)+1];
    sp[4 *(y * sw + x)+2] = intensity * tp[4*(texHeight * texY + xt)+2];
    //sp[4 *(y * sw + x)+3] = tp[4*(texHeight * texY + xt)+3];

  } 
}

*/

 


/* 
// This code is more directly taken from "Gardens of Imagination"
// But I get even less satisfying results with it. 
//  (relative to visual appearance)
void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text) {


  int sh = surf->h; 
  int sw = surf->w;

  int t = xt; 

  int height = y2 - y1; 

  // int dheight = height; 
  int iheight = 64;

  float yratio = 64.0 / height; 

  if (y1 < 0){ // clip 
    // dheight -= (0 - y1);
    t += (int)((-y1)*yratio)*64;
    iheight -= ((-y1) * yratio);
    y1 = 0; 
  }
  
  if (y2 > 199) { // clip 
    // dheight -= (y2 - 199);
    iheight -= (y2 - 200) * yratio;
    // y2 = 200;
  }
  
  int offset = y1 * 320 + x; 
  int texOffs = t; 
  
  // assume 32bit ints.. (fix) 
  unsigned int *sp =(unsigned int*)surf->pixels;
  unsigned int *tp =(unsigned int*)text->pixels;  

  int error = 64;
  int h; 
  for (h = 0; h <= iheight; h ++) { 
    
    while (error >= 64) {

      sp[offset] = tp[texOffs];
      error -= 64;
      offset += 320; 
    }
    error += height; 
    texOffs += 64;
  }
} 
*/ 
