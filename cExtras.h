
#include <SDL/SDL.h>

typedef struct { 
  int32_t lx;
  int32_t ly;
  float   inR; 
  float   inG; 
  float   inB;
} light; 




void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text);

void texturedVLineLit(int x, int y1, int y2, SDL_Surface *surf,
		      int xt, int yt1, int yt2, SDL_Surface *text, 
		      float intensityR,
		      float intensityG, 
		      float intensityB);

void renderRItem(int x, int y, int w, int h, SDL_Surface *surf, 
                 SDL_Surface *text, 
		 float inR,
                 float inG, 
                 float inB, 
		 float depth, float *depths);
  
void texPoint(int tx, int ty, int tw, int32_t *text,
              int x, int y, int w, int32_t *surf, 
              float inR, float inG, float inB); 

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
	     float   p2x, float p2y);
