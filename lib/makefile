
all: test

cailie.o: cailie.cpp cailie.h cailie_threads.h cailie_internal.h
	g++ -c cailie.cpp

cailie_threads.o: cailie_threads.cpp cailie.h cailie_threads.h cailie_internal.h
	g++ -c cailie_threads.cpp

libcailie.a: cailie.o cailie_threads.o
	ar rcs libcailie.a cailie.o cailie_threads.o

test: test.cpp libcailie.a
	g++ test.cpp -L. -lcailie -o test -lpthread

clean:
	rm -f libcailie.a test cailie.o cailie_threads.o
