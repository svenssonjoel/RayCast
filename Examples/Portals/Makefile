
all: main

main: Main.hs 
	ghc Main.hs ../../cExtras.o -o main -O3  -lm -i../../

clean: 
	rm *.hi
	rm *.o 
	rm main