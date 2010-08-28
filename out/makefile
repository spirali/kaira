
all: project

project.cpp: project.xml
	../ptp/ptp project.xml project.cpp

project: project.cpp
	g++ project.cpp -o project -L../lib -lcailie -I../lib -ggdb -O0 -lpthread

clean: 
	rm -f project project.cpp
