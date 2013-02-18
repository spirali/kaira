if [ -f makefile ]; then
	make clean
fi

if [ -f makefile.main ]; then
	make -f makefile.main clean
fi

rm -fr makefile *.xml *.log *.klog *.kreport server *.ktt *.kth
