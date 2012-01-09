/* This file is included at the beginning of the main source file,
   so definitions from this file can be used in functions in
   transitions and places. */
   
#include <assert.h>
#include <sstream>

class Matrix {

	public:
	
	Matrix() : size_x(0), size_y(0), array(NULL) {}
	
	Matrix(const Matrix &matrix) {
		if (matrix.array) {
			size_x = matrix.size_x;
			size_y = matrix.size_y;
			array = new int[matrix.size_x * matrix.size_y];
			set_data(matrix.array);
		} else {
			size_x = 0;
			size_y = 0;
			array = NULL;
		}
	}
	
	Matrix & operator= (const Matrix & matrix) {
		if (matrix.array) {
			if (size_x * size_y != matrix.size_x * matrix.size_y) {
				if (array) {
					delete array;
				}
				array = new int[matrix.size_x * matrix.size_y];
			}
			size_x = matrix.size_x;
			size_y = matrix.size_y;
			set_data(matrix.array);
		} else {
			if (array)
				delete [] array;
			size_x = 0;
			size_y = 0;
			array = NULL;
		}		
		return *this;
	}
	
	int get_rows_count() const { return size_y; }
	int get_columns_count() const { return size_x; }
	
	std::vector<int> get_row(int r) const {
		std::vector<int> v(size_x);
		for (int i = 0; i < size_x; i++) {
			v[i] = array[i + r * size_x];
		}
		return v;
	}
	
	std::vector<int> get_column(int c) const {
		std::vector<int> v(size_y);
		for (int i = 0; i < size_y; i++) {
			v[i] = array[c + i * size_x];
		}
		return v;
	}
			
	Matrix(int size_y, int size_x) : size_x(size_x), size_y(size_y) {
		array = new int[size_x * size_y];
	}
	
	void set_data(std::vector<int> &data) {
		int s = size_x * size_y;
		assert(s == data.size());
		for (int i = 0; i < s; i++) {
			array[i] = data[i];
		}
	}
	
	void set_data(int *data) {
		int s = size_x * size_y;
		for (int i = 0; i < s; i++) {
			array[i] = data[i];
		}
	}
	
	
	std::string as_string() const {
		if (array == NULL) {
			return "matrix(empty)";
		}
		std::stringstream s;
		s << "matrix(" << size_x << "," << size_y << ",[";
		int j;
		for (j = 0; j < size_y * size_x; j++) {
			if (j % size_x == 0) {
				s << "[" << array[j];
			} else {
				s << "," << array[j];
			}
			if ((j + 1) % size_x == 0) {
				s << "]";
			}
		}
		s << "]";
		return s.str();
	}
	
	virtual ~Matrix() {
		if (array)
			delete [] array;
	}

	protected:
	int size_y;
	int size_x;
	int *array;
};