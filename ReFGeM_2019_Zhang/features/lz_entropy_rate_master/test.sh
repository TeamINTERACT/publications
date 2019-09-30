#!/bin/bash

make -f Makefile clean
make -f Makefile

# ./Debug/lzEntropy testInput/entropyIn_P2_T600_W1.csv > test.out
./Debug/lzEntropy testInput/smalltest.csv


