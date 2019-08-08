/*
 * Please report any problem to the author:
 * +----------------------------------+
 * | Author: Tuhin Paul               |
 * |  Email: tup500@mail.usask.ca     |
 * |  Phone: 306 880 5494             |
 * +----------------------------------+
 *
 */

#include <cstdio>       /* isspace() */
#include <iostream>
#include <string>
#include <math.h>       /* log2 */
#include <fstream>
#include <cstdlib>
#include <cctype>       /* isdigit */

#include "LZEntropy.h"
#include "Row.h"
#include "util.h"

using namespace std;

int debugMode = false;

int main (int argc, char* argv[])
{
	/*
	LZEntropy<char> lzH;

	string s("ABC ABCDAB ABCDABCDABDE");
	string w("ABCDABD");
	string x("ABXYPQMN");
	Row<char>* S = string2charrows(s);
	Row<char>* W = string2charrows(w);
	Row<char>* X = string2charrows(x);
	cout << lzH.lzEntropy( S, s.length() ) << endl;
	cout << lzH.lzEntropy( W, w.length() ) << endl;
	cout << lzH.lzEntropy( X, x.length() ) << endl;
	
	print_rows( S, s.length() );
	*/
	
	if  ( argc != 2 )
	{
		string eMsg = "There should be exactly one argument (input file path)!";
		cerr << "ERROR: " << eMsg << endl;
		throw eMsg;
	}

	char iFDelimiter = ',';

	// file path
	string fPath(argv[1]);
	
	long numLines = 0;
	FILE *infile = fopen( fPath.c_str(), "r" );
    int ch;
	int chLast;
    while ( EOF != (ch=getc(infile)) )
	{
		chLast = ch;
        if ( (int)'\n' == ch )
            ++numLines;
	}
	
	if ( chLast != (int)'\n' )
		++numLines;
	
	Row<long> *rows = new Row<long>[numLines - 1]; // because there should be a header

	int numRecords = 0; // actual num of records to consider
	                    // empty lines in the input file are ignored

	string line;
	ifstream myfile ( fPath.c_str() );
	if (myfile.is_open())
	{
		// ignore header
		getline (myfile, line);

		int rNo = 0;
		while ( getline (myfile,line) )
		{
			Row<long> *curRowHead = NULL; // anchor to the current set of numbers
			Row<long> *curRow = NULL;     // moving pointer to add new numbers
			
			bool inAWord = false;      // flag to scan if we are inside a word during scanning the line

			bool containsWord = false; // flag to check if there's any (numerical) word in the line.
			                           // if no word is contained, the line will be dropped.
			
			int iLinePrev = 0; // index of previous non-space character
			int iLine = 0;     // index of current scanned character
			for( ; line[iLine] != '\0'; iLine++ )
			{
				if ( isspace(line[iLine]) || line[iLine] == iFDelimiter )
				{
					// space found

					if (inAWord)
					{
						// whitespace starts after a word (should be numerical word)

						// get the number from the word:
						long x = atol( line.substr(iLinePrev, iLine-iLinePrev).c_str() );

						// if the set of current numbers is empty, then initialize
						// OR
						// add next node to the linked list
						if (curRowHead == NULL)
						{
							curRowHead = new Row<long>;
							curRow = curRowHead;
						}
						else
						{
							curRow->addNext();
							curRow = curRow->getNext();
						}

						// update the default current value with the found number
						curRow->setVal(x);

						// we are not in any word now
						inAWord = false;
						
					}
					else
					{
						// continuation of whitespace
						// just ignore
					}
				}
				else
				{
					// non-space character found
					
					if (! isdigit(line[iLine]) )
					{
						string eMsg = "Non-digit character found in the line";
						cerr << "ERROR (Line " << __LINE__ << "): " << eMsg << endl;
						throw eMsg;
					}


					if (inAWord)
					{
						// continuation of a word
						// do nothing - address the word when a whitespace OR '\0' terminates the word
					}
					else
					{
						// a new word starts

						inAWord = true;
						
						// store the index where this new word starts
						iLinePrev = iLine;
						
						// there's a word at least- so don't drop the line
						if (! containsWord)
							containsWord = true;
					}
				}
			}
			
			// If there was an (unprocessed) word before '\0':
			if (inAWord)
			{
				// get the number from the word:
				long x = atol( line.substr(iLinePrev, iLine-iLinePrev).c_str() );

				// if the set of current numbers is empty, then initialize
				// OR
				// add next node to the linked list
				if (curRowHead == NULL)
				{
					curRowHead = new Row<long>();
					curRow = curRowHead;
				}
				else
				{
					curRow->addNext();
					curRow = curRow->getNext();
				}

				// update the default current value with the found number
				curRow->setVal(x);

				// we don't need to modify [inAWord] flag here
			}
			
			if ( containsWord ) // at least one number found in current line
			{
				curRow = curRowHead;
				
				Row<long> *rowInArr = &(rows[rNo]);
				if (rowInArr == NULL)
				{
					
					string eMsg = "rowInArr should not be null";
					cerr << "ERROR (rNo = " << rNo << "): " << eMsg << endl;
					throw eMsg;
				}
				
				while (curRow != NULL)
				{
					// update value in the array at the current index
					rowInArr->setVal( curRow->getVal() );
					
					// IF there is an element next:
					// add a new node to the row in the array
					if ( curRow->getNext() != NULL )
					{
						rowInArr->addNext();
						rowInArr = rowInArr->getNext();
					}

					// advance curRow
					curRow = curRow->getNext();					
				}
				
				// number of nonempty rows
				rNo++;
			}
		}

		numRecords = rNo;
		myfile.close();
	}
	
	if ( debugMode )
	{
		cout << "# of actual records: " << numRecords << endl;
		print_rows(rows, numRecords);
		cout << endl;
	}

	LZEntropy<long> lzH;
	cout << lzH.lzEntropy( rows, numRecords );

	return 0;
}

