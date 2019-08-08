/*
 * Please report any bug to the author:
 * +----------------------------------+
 * | Author: Tuhin Paul               |
 * |  Email: tup500@mail.usask.ca     |
 * |  Phone: 306 880 5494             |
 * +----------------------------------+
 *
 */
 
#ifndef ROW_H
#define ROW_H

#include <cstddef>    /* NULL */

template<class T>
bool equals ( T &a, T &b );

template <class T>
class Row
{
private:
	T val;
	Row<T> *next;
public:
	Row();
	~Row();

	bool operator == (Row<T> b);
	bool operator != (Row<T> b);
	
	void addNext();
	Row<T>* getNext();

	T getVal();
	void setVal ( T val );
};

/************ IMPLEMENTATION ************/

template<class T>
bool equals ( T &a, T &b )
{
	return a == b; // todo: make type safe ( e.g., what if T is string? )
}

template <class T>
Row<T>::Row() :
	next(NULL)
{
}

template <class T>
Row<T>::~Row()
{
}

template <class T>
bool Row<T>::operator == (Row<T> b)
{
	bool eq = equals ( this->val, b.val )
			  && 
			  (
				  ( this->next == NULL && b.next == NULL )
				  ||
				  (
					  ( this->next != NULL ) && ( b.next != NULL ) &&
					  ( *(this->next) == *(b.next) )
				  )
			  );

	return eq;
}

template <class T>
bool Row<T>::operator != (Row<T> b)
{
	return !(*this == b);
}

template <class T>
void Row<T>::addNext()
{
	this->next = new Row<T>();
}

template <class T>
Row<T>* Row<T>::getNext()
{
	return this->next;
}

template <class T>
T Row<T>::getVal()
{
	return this->val;
}

template <class T>
void Row<T>::setVal ( T val )
{
	this->val = val;
}

#endif // ROW_H

