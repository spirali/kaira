
CFLAGS=-ggdb
CC=g++
MPICC=mpicc

all: test

mpi: libmpicailie.a

cailie_mpi.o: cailie_mpi.cpp cailie_mpi.h
	${MPICC} -c cailie_mpi.cpp cailie_mpi.h ${CFLAGS} -cc=g++

mpicailie.o: cailie.cpp cailie.h
	${MPICC} -c cailie.cpp -o mpicailie.o -DCA_MPI ${CFLAGS} -cc=g++

%.o: %.cpp %.h
	${CC} -c $*.cpp ${CFLAGS}

libcailie.a: cailie.o cailie_threads.o
	ar rcs libcailie.a cailie.o cailie_threads.o

libmpicailie.a: mpicailie.o cailie_mpi.o
	ar rcs libmpicailie.a mpicailie.o cailie_mpi.o

test: test.cpp libcailie.a
	${CC} test.cpp -L. -lcailie -o test -lpthread -lrt

clean:
	rm -f libmpicailie.a libcailie.a test *.o
