.PHONY: all clean
all:
	test -d Debug || mkdir -p Debug
	g++ -Wall -c -o Debug/Row.o       Row.h
	g++ -Wall -c -o Debug/util.o      util.h
	g++ -Wall -c -o Debug/LZEntropy.o LZEntropy.h
	g++ -Wall    -o Debug/lzEntropy   main.cpp      -I .
 
clean:
	rm -rf Debug/*
