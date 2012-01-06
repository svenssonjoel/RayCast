
#include <SDL/SDL.h> 

#include <stdio.h>


/* 
  as direct a Haskell -> C translation as I could come up 
  with. 
   + Directly lead to huge performance gain.
   - Why is the Haskell version SO extremely slow ? 
*/

void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text) {

  int i; 
  int sh = surf->h; 
  int sw = surf->w;
  int clipped_y1 = y1 > 0 ? y1 : 0;
  int clipped_y2 = y2 < sh ? y2 : (sh - 1);
  int lineHeight = y2 - y1; 
  int clippedHeight = clipped_y2 - clipped_y1;
  int texHeight = yt2 - yt1; 
  float ratio = (float)(texHeight-1) / lineHeight;

  int clipped_yt1 = yt1 + (int)((clipped_y1 - y1) * ratio);
  

  int start  = clipped_y1 * sw + x; 
  int tstart = clipped_yt1 * 64 + xt;  

  // assume 32bit ints.. (fix) 
  unsigned int *sp =(unsigned int*)surf->pixels;
  unsigned int *tp =(unsigned int*)text->pixels;  


  for (i = 0; i <= clippedHeight; i ++)  {
    unsigned int p = tp[tstart + ((int)(i*ratio)*64)];
    sp[start + (i*sw)] = p; 
  }  
    

} 


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
