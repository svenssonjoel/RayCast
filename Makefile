


all: main


main: Main.hs SDLUtils.hs CExtras.hs cExtras.c
	ghc Main.hs cExtras.c -o main -O3 

CExtras.hs: CExtras.chs 
	c2hs CExtras.chs

clean: 
	rm *.hi
	rm *.o 
	rm main