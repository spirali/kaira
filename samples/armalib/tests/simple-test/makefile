BUILD_DIR=../../lib

simple-test: simple-test.cpp
	$(CXX) -o simple-test simple-test.cpp -I$(BUILD_DIR) -L$(BUILD_DIR) $(CFLAGS) $(INCLUDE) $(LIBDIR) -larmalib $(LIBS)

include $(BUILD_DIR)/makefile
