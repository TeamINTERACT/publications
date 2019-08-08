Please report any bug to the author:

Author: Tuhin Paul

Email: tup500@mail.usask.ca

Phone: 306 880 5494



Last updated: Jan 16, 2017
--------------------------


What this program does:
-----------------------

Calculates LZ entropy rate of a sequence of 2D coordinates whose elements have long data type.
I ran this program on linux but this program should run okay on windows/mac.
First line of the input file is assumed to be the header, and is ignored in entropy rate calculation.
Obviously if you have different numbers of elements on different lines, your header may not match a line later in the file.
The program now supports differnt numbers of numbers in different lines.
Comma and whitespace characters (except '\r' and '\r\n') are used as number delimiters.
A line with no numbers is ignored.
If a line has non-digit character that is not a delimiter or newline character, an exception will be thrown.

Learn about the LZ entropy rate calculation from the supporting online material for the following paper.
This is not the primary source of the entropy rate equation. In this paper, they mention entropy, not entropy rate.

> Song, Chaoming, Zehui Qu, Nicholas Blumm, and Albert-László Barabási. "Limits of predictability in human mobility." Science 327, no. 5968 (2010): 1018-1021.


### How to compile and link:

>	make

### How to clean:

>	make clean

### How to run:
	./Debug/lzEntropy <input_file>


Limitation(s):
--------------

Newline character ('\n') is considered as the line separator.
Not tested in Windows OS, where the line separator is "\r\n".
If you have an input file created in windows, you may use the dos2unix command in linux to convert "\r\n" to '\n'.
It will not work properly if the line separator is '\r'.


Naming confusion:
-----------------

Each line is represented as a Row instance. Each line of the input file is stored as a linked list of Row type. In a row, each element is actually a Row instance (as of now).
It probably makes better sense if the elements of a row are named something like cell or node.
For now, I would leave it as it is.

