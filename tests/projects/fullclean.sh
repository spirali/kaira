if [ -f makefile ]; then
	make clean
fi

if [ -f makefile.main ]; then
	make -f makefile.main clean
fi

rm -f makefile *.xml *.log *.klog
