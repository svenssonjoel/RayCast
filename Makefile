
ENGINEFILES = ./Engine/Math.hs \
              ./Engine/Map.hs \
              ./Engine/RayCast.hs

all: main


main: Main.hs SDLUtils.hs CExtras.hs MathExtras.hs cExtras.c $(ENGINEFILES) 		
	ghc Main.hs cExtras.c -o main -O3  -lm

CExtras.hs: CExtras.chs 
	c2hs CExtras.chs

clean: 
	rm *.hi
	rm *.o 
	rm main