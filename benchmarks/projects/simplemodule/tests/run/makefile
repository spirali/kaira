BUILD_DIR=../../release

run: run.cpp
	$(CC) -o run run.cpp -I$(BUILD_DIR) -L$(BUILD_DIR) $(CFLAGS) $(INCLUDE) $(LIBDIR) -lsimplemodule $(LIBS)

include $(BUILD_DIR)/makefile
