
#include <SDL/SDL.h>

typedef struct { 
  float   lx;
  float   ly;
  float   inR; 
  float   inG; 
  float   inB;
} light; 


typedef struct { 
  int32_t x; 
  int32_t y; 
} Point2D_int; 

typedef struct { 
  float x; 
  float y; 
} Point2D;

typedef struct { 
  int32_t w; 
  int32_t h; 
} Dims2D_int; 

typedef struct { 
  float x1;
  float y1; 
  float x2;
  float y2; 
} RealLine; 

typedef struct { 
  float r; 
  float g; 
  float b; 
} RGB;

typedef struct { 
  int32_t viewDist;
  int32_t viewHeight;
  int32_t windowWidth;
  int32_t windowHeight; 
  int32_t wallWidth; 
  int32_t wallHeight;
} ViewConfig; 


void texturedVLine(int x, int y1, int y2, SDL_Surface *surf,
		   int xt, int yt1, int yt2, SDL_Surface *text);

void texturedVLineLit(int32_t x, int32_t y1, int32_t y2, SDL_Surface *surf,
		      int32_t xt, int32_t yt1, int32_t yt2, SDL_Surface *text, RGB *rgb);  
//		      float intensityR,
//		      float intensityG, 
//		      float intensityB);

void texturedVLineLit3S(int32_t x, int32_t y0, int32_t y1, SDL_Surface *surf,
		        int32_t tc1,
			int32_t tc2, 
			int32_t tc3, int32_t yt0, int32_t yt1, 
			SDL_Surface *t1,
			SDL_Surface *t2,
			SDL_Surface *t3,
		        RGB *rgb);


void renderRItem(Point2D_int *p, Dims2D_int *dim, SDL_Surface *surf, 
                 SDL_Surface *text, 
		 float depth, float *depths,
		 Point2D *wp, 
		 light *lights, 
		 int32_t num_lights);

  
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


void lerpRows(//int32_t wallWidth, 
	      //int32_t modMask,
	      //int32_t windowWidth, // things from ViewConfig that matters to this fun
              ViewConfig *vc,
	     
	      int32_t mapW, 
	      int32_t mapH, 
	      int32_t *map,
	     
	      int32_t *bots,
              int32_t **textures,
	      SDL_Surface *surf,
	      light *lights, 
	      int32_t num_lights,
	      
              RealLine *rl);
	      // float   *p1xa, float *p1ya, 
	      //float   *p2xa, float *p2ya);

void computeLight(float *r,
		  float *g,
                  float *b,
		  int32_t px,
		  int32_t py, 
		  light *lights,
		  int32_t num_lights
		  );
