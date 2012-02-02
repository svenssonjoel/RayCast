
#include <SDL/SDL.h>

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
  
