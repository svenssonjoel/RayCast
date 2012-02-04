
ENGINEFILES = ./Engine/Math.hs \
              ./Engine/Map.hs \
              ./Engine/RayCast.hs \
              ./Engine/Render.hs \
              ./Engine/RItem.hs \
              ./Engine/Sprite.hs

all: main


main: Main.hs SDLUtils.hs CExtras.hs MathExtras.hs cExtras.o $(ENGINEFILES) 		
	ghc Main.hs cExtras.o -o main -O3  -lm 

cExtras.o :: cExtras.c 
	gcc -Wall -c cExtras.c -o cExtras.o -O3 -funroll-all-loops -frerun-cse-after-loop


CExtras.hs: CExtras.chs 
	c2hs CExtras.chs

clean: 
	rm *.hi
	rm *.o 
	rm main