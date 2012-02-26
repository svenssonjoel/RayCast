

all: CExtras.hs cExtras.o 

cExtras.o :: cExtras.c 
	gcc -Wall -c cExtras.c -o cExtras.o -O3 -funroll-all-loops -frerun-cse-after-loop


CExtras.hs: CExtras.chs 
	c2hs CExtras.chs

clean: 
	rm CExtras.hs
	rm cExtras.o