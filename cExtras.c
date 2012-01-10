
#include <SDL/SDL.h> 

// #include <stdio.h>

void texturedVLine(int x, int y0, int y1, SDL_Surface *surf,
		   int xt, int yt0, int yt1, SDL_Surface *text) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
  int th = text->h;
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
 

void texturedVLineLit(int x, int y0, int y1, SDL_Surface *surf,
		      int xt, int yt0, int yt1, SDL_Surface *text, float intensity) {

  int y; 
  int sh = surf->h; 
  int sw = surf->w;
  int th = text->h;
  int clipped_y1 = y0 > 0 ? y0 : 0;
  int clipped_y2 = y1 < sh ? y1 : sh;
 
  int lineHeight = y1 - y0; 
  int texHeight = yt1 - yt0; 

  unsigned char *sp =(unsigned char*)surf->pixels;
  unsigned char *tp =(unsigned char*)text->pixels;  

  float ratio = (float)texHeight / lineHeight;
  
  for(y = clipped_y1; y<clipped_y2; y++){
    
    float ty = (y - y0) * ratio;


    sp[4 *(y * sw + x)]   = intensity * tp[4*(texHeight * (int)ty + xt)];
    sp[4 *(y * sw + x)+1] = intensity * tp[4*(texHeight * (int)ty + xt)+1];
    sp[4 *(y * sw + x)+2] = intensity * tp[4*(texHeight * (int)ty + xt)+2];
    //sp[4 *(y * sw + x)+3] = tp[4*(texHeight * texY + xt)+3];

  } 
}



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
