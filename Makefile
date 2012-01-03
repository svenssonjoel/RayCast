


all: main


main: Main.hs
	ghc Main.hs -o main -O3 