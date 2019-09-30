/*
 * Please report any problem to the author:
 * +----------------------------------+
 * | Author: Tuhin Paul               |
 * |  Email: tup500@mail.usask.ca     |
 * |  Phone: 306 880 5494             |
 * +----------------------------------+
 * 
 * KMP implementation for matrix comparison
 * REFERENCES:
 *     https://www.youtube.com/watch?v=GTJr8OvyEVQ
 *     https://www.cs.ubc.ca/~hoos/cpsc445/Handouts/kmp.pdf
 * 
 * ISSUES:
 *     C++ Template separate implementation issue:
 *         http://www.codeproject.com/Articles/48575/How-to-define-a-template-class-in-a-h-file-and-imp
 *         http://www.teamavolition.com/topic/13695-solved-c-undefined-reference-to/
 */

#ifndef LZENTROPY_H
#define LZENTROPY_H

#include <math.h>
#include "Row.h"
#include <iostream>
#include <iomanip>      /* std::setw */

extern int debugMode;

template<class T>
class LZEntropy {
public:
    LZEntropy();
    ~LZEntropy();

private:
    int* make_pmtable(Row<T> *rows, int len);
    int search(Row<T> *text, int textLen, Row<T> *pattern, int patternLen);

public:
    double lzEntropy(Row<T> *seq, int seqLen);
};

/************ IMPLEMENTATION ************/

template<typename T>
LZEntropy<T>::LZEntropy() {
}

template<typename T>
LZEntropy<T>::~LZEntropy() {
}

template<typename T>
int* LZEntropy<T>::make_pmtable(Row<T> *rows, int len) {
    int* pmTable = new int[len];

    pmTable[0] = 0;
    for (int j = 0, i = 1; i < len;) {
        if (rows[i] == rows[j]) {
            pmTable[i] = ++j;
            ++i;
        } else {
            if (j > 0)
                j = pmTable[j - 1];
            else {
                pmTable[i] = 0;
                ++i;
            }
        }
    }

    return pmTable;
}

template<typename T>
int LZEntropy<T>::search(Row<T> *text, int textLen, Row<T> *pattern, int patternLen) {
    int * pmTable = make_pmtable(pattern, patternLen);

    int i = 0; // main text index
    int q = 0; // pattern index

    int matchAt = -1;
    while (i < textLen) {
        if (text[i] == pattern[q]) {
            i++;
            q++;

            if (q == patternLen) {
                matchAt = i - q;
                break;
            }
        } else {
            if (q == 0)
                i++;
            else
                q = pmTable[q - 1];
        }
    }

    delete pmTable;

    return matchAt;
}

template<typename T>
double LZEntropy<T>::lzEntropy(Row<T> *seq, int seqLen) {
    if (debugMode) {
        std::cout << std::endl;
        std::cout << "at: " << std::setw(4) << 0 << " Len: " << std::setw(4) << 1 << " Sum(Lambda): " << std::setw(6) << 1 << std::endl;
    }

    long lambda = 1;
    for (int i = 1; i < seqLen; i++) {
        Row<T>* S = seq;
        int sLen = i; // 0 to i-1

        Row<T>* W = &seq[i];
        int jMax = fmin(seqLen - 1, 2 * i);
        for (int j = i; j <= jMax; j++) {
            int wLen = j - i + 1;

            int foundAt = search(S, sLen, W, wLen);

            if (foundAt < 0) // if not found
            {
                lambda += (j - i + 1);

                if (debugMode)
                    std::cout << "at: " << std::setw(4) << i << " Len: " << std::setw(4) << wLen << " Sum(Lambda): " << std::setw(6) << lambda << std::endl;

                break;
            }
        }
    }

    if (debugMode) {
        std::cout << std::endl;
        std::cout << "seqLen = " << seqLen << std::endl << "lambda = " << lambda << std::endl;
    }

    double H = (1.0 * seqLen / lambda) * log2(seqLen);
    return H;
}

#endif // LZENTROPY_H
