
CFLAGS=-ggdb
CC=g++
MPICC=mpicc

all: libcailie.a

mpi: libmpicailie.a

#cailie_mpi.o: cailie_mpi.cpp cailie_mpi.h
#	${MPICC} -c cailie_mpi.cpp cailie_mpi.h ${CFLAGS} -cc=g++

#mpicailie.o: cailie.cpp cailie.h
# 	${MPICC} -c cailie.cpp -o mpicailie.o -DCA_MPI ${CFLAGS} -cc=g++

%.o: %.cpp %.h
	${CC} -c $*.cpp ${CFLAGS}

libcailie.a: cailie.o unit.o path.o process.o messages.o
	ar rcs libcailie.a $^

#libmpicailie.a: mpicailie.o cailie_mpi.o
#	ar rcs libmpicailie.a mpicailie.o cailie_mpi.o

clean:
	rm -f libmpicailie.a libcailie.a test *.o
