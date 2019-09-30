/*
 * Please report any bug to the author:
 * +----------------------------------+
 * | Author: Tuhin Paul               |
 * |  Email: tup500@mail.usask.ca     |
 * |  Phone: 306 880 5494             |
 * +----------------------------------+
 *
 */

#ifndef UTIL_H
#define UTIL_H

#include "Row.h"
#include <string>
#include <iostream>

using namespace std;

template<class T> void print_rows  (Row<T> *rows, int len);
Row<char>*             string2rows (string str);


template<class T>
void print_rows ( Row<T> *rows, int len)
{
	for ( int i = 0; i < len; i++ )
	{
		Row<T> *tail = &rows[i];
		
		if ( tail != NULL )
		{
			cout << tail->getVal();
			tail = tail->getNext();
		}
		while ( tail != NULL )
		{
			cout << " " << tail->getVal();
			tail = tail->getNext();
		}
		cout << endl;
	}
}

Row<char>* string2charrows(string str)
{
	int L = str.length();
	Row<char> *rows;
	rows = new Row<char>[L];
	for ( int i = 0; i < L; i++ )
	{
		rows[i].setVal( str[i] );
	}
	return rows;
}

#endif // UTIL_H
