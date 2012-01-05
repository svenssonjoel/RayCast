

#include <SDL/SDL.h> 

#include <stdio.h>

void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text) {



  int i; 
  int sh = surf->h; // SDL_surfaceGetWidth(surf);
  int sw = surf->w;
  int clipped_y1 = y1 > 0 ? y1 : 0;
  int clipped_y2 = y2 < (sh-1) ? y2 : sh - 1;
  int lineHeight = y2 - y1; 
  int clippedHeight = clipped_y2 - clipped_y1;
  int texHeight = yt2 - yt1; 
  float ratio = (float)texHeight / lineHeight;

  int clipped_yt1 = yt1 + (int)((clipped_y1 - y1) * ratio);

  int start  = clipped_y1 * sw + x; 
  int tstart = clipped_yt1 * 64 + xt;  

  //printf("entered c function\n %d %d\n", lineHeight, clippedHeight);
  
  // assume 32bit ints.. (fix) 
  unsigned int *sp =(unsigned int*)surf->pixels;
  unsigned int *tp =(unsigned int*)text->pixels;  


  for (i = 0; i < clippedHeight; i ++)  {
    //printf ("loop %d\n", i);
    unsigned int p = tp[tstart + ((int)(i*ratio))*64];
    sp[start + (i*sw)] = p; 
  }  
    

} 

/*
texturedVLine x y1 y2 surf xt yt1 yt2 tex = 
  do 
    
    texPix  <- castPtr `fmap` surfaceGetPixels tex
    surfPix <- castPtr `fmap` surfaceGetPixels surf 
    sequence_ [ do
                   
      
              | i <- [0..clippedHeight]
              ]    
	      
  where 
    sw = surfaceGetWidth surf
    sh = surfaceGetHeight surf
    clipped_y1  = max 0 y1
    clipped_y2  = min y2 (sh-1)
    clipped_yt1 = yt1 + floor (fromIntegral (clipped_y1 - y1) * ratio)   
    
    start  = clipped_y1 * sw + x 
    tstart = clipped_yt1 * 64 + xt  
  
    lineHeight = y2 - y1 
    clippedHeight = clipped_y2 - clipped_y1 
    texHeight  = yt2 - yt1 
    ratio      = (fromIntegral texHeight) /  (fromIntegral lineHeight) 
    
*/    
