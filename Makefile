# Commands:

name := foo

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o play Main.hs

prof:
	ghc --make -prof -o play Main.hs

all: build

# Cleaning commands:
clean:
	rm -f play
	rm -f *.hi
	rm -f *.o
