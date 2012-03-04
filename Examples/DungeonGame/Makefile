
ENGINEFILES = ../../Engine/Math.hs \
              ../../Engine/Render.hs \
              ../../Engine/RItem.hs \
              ../../Engine/Sprite.hs \
              ../../Engine/ZBuffer.hs \
              ../../Engine/Light.hs \
              ../../Engine/ViewConfig.hs \
              ../../Engine/Slice.hs \
              ../../Engine/World.hs \
              ../../Engine/CubeWorld/Map.hs \
              ../../Engine/CubeWorld/RayCast.hs \
              ../../CExtras.hs

all: main 

main: Main.hs $(ENGINEFILES) ../../cExtras.o ../../CExtras.hs
	ghc Main.hs ../../cExtras.o -o main -O3  -lm -i../../

../../cExtras.o: ../../cExtras.c 
	gcc -std=c99 -c ../../cExtras.c -o ../../cExtras.o -Wall -pedantic -O3 -funroll-all-loops

../../CExtras.hs: ../../CExtras.chs
	c2hs ../../CExtras.chs  

clean: 
	rm *.hi
	rm *.o 
	rm main