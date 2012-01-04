


all: main


main: Main.hs SDLUtils.hs
	ghc Main.hs -o main -O3 

clean: 
	rm *.hi
	rm *.o 
	rm main