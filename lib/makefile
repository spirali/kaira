
all: test

%.o: %.cpp %.h
	g++ -c $*.cpp -ggdb

libcailie.a: cailie.o cailie_threads.o cailie_sim.o
	ar rcs libcailie.a cailie.o cailie_threads.o cailie_sim.o

test: test.cpp libcailie.a
	g++ test.cpp -L. -lcailie -o test -lpthread

clean:
	rm -f libcailie.a test *.o
