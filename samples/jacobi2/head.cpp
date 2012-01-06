
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <iomanip>

#define WIDTH parameter_SIZE_X()
#define ROW_DATA_SIZE (sizeof(double) * WIDTH)

int parameter_WORKERS();
int parameter_SIZE_Y();
int parameter_SIZE_X();
int parameter_TEMP();

class Row {
	
	public:

	Row() {
		array = new double[WIDTH];
	}

	Row(const double *data) {
		array = new double [WIDTH];
		set_raw_data(data);
	}

	~Row() {
		delete [] array;
	}

	Row(const Row &c) {
		array = new double [WIDTH];
		set_raw_data(c.get_raw_data());
	}

	Row & operator= (const Row & c) {
        if (this != &c)
		{
			set_raw_data(c.get_raw_data());
        }
        return *this;
	}


	const double * get_raw_data() const { return array; }
	void set_raw_data(const double *data) {
		memcpy(array, data, ROW_DATA_SIZE);
	}

	void set_zero() {
		int t;
		for (t = 0; t < WIDTH; t++) {
			array[t] = 0.0;
		}
	}

	void dump() {
		int t;
		printf("Column: ");
		for (t = 0; t < WIDTH; t++) {
			printf("%g ", array[t]);
		}
		printf("\n");
	}

	public:
		double *array;

};

class Data {
	public:
		Data(const int position, const int size) {
			this->size = size;
			this->position = position;
			array1 = new double[(size + 2) * (WIDTH + 2)];
			array2 = new double[(size + 2) * (WIDTH + 2)];
			init();
		}
		
		void init() {
			int t;
			for (t = (WIDTH + 2); t < (size + 2) * (WIDTH + 2); t++) {
				array1[t] = 0.0;
			}
			set_fixed_temp();
		}

		double * get_raw_row(int i) const { return array1 + (i + 1) * (WIDTH + 2) + 1; };
		void set_raw_row(int i, const double *data) { memcpy(get_raw_row(i), data, ROW_DATA_SIZE); };		


		void set_fixed_temp()
		{
			int source_y = parameter_SIZE_Y() / 2;
			if (source_y >= position && source_y < position + size) {
				source_y -= position;
				int source_x = parameter_SIZE_X() / 2;
				array1[ (source_y + 1) * (WIDTH + 2) + (source_x + 1) ] = parameter_TEMP();
			}
		}
		
		void compute(Row &c1, Row &c2)
		{
			set_raw_row(-1, c1.get_raw_data());
			set_raw_row(size, c2.get_raw_data());			
			int t, s;
			for (t = WIDTH + 2; t < (size + 1) * (WIDTH + 2); t+=(WIDTH + 2)) {
				array1[t] = 0.0; // Zero values in left and right columns
				array1[t + WIDTH + 1] = 0.0;
				for (s = 1; s <= WIDTH; s++) {
					int i = t + s;
					int u = i - (WIDTH + 2);
					int d = i + (WIDTH + 2); 
					int l = i - 1;
					int r = i + 1;
					array2[i] = (4.0 * array1[i] + array1[u] + array1[d] + array1[l] + array1[r]) / 8.0;
				}
			}
			double *a = array1;
			array1 = array2;
			array2 = a;
			set_fixed_temp();
			get_rows(c1, c2);
		}

		void get_rows(Row &c1, Row &c2) 
		{
			c1.set_raw_data(get_raw_row(0));
			c2.set_raw_data(get_raw_row(size -1));
		}

		std::string as_html() {
			std::stringstream stream;
			stream.precision(2);
			stream.setf(std::ios_base::fixed);
			for (int t = WIDTH + 2; t < (size + 1) * (WIDTH + 2); t+=(WIDTH + 2)) {
				stream << "<tr>" << std::endl;
				for (int s = 1; s <= WIDTH; s++) {
					int i = t + s;
					stream << "<td>" << array1[i] << "<td/>" << std::endl;
				}
				stream << "</tr>" << std::endl;
			}
			return stream.str();
		}

		~Data() {
			delete [] array1;
			delete [] array2;
		}
		
		int get_size() const { return size; }

	protected:
		int size;
		int position;
		double *array1, *array2;
};

int id_to_position(int process_count, int iid)
{
	int rows_per_instance = ((parameter_SIZE_Y() - 1) / process_count) + 1;
	return rows_per_instance * iid;
}

int id_to_size(int process_count, int iid)
{
	int rows_per_instance = ((parameter_SIZE_Y() - 1) / process_count) + 1;
	int first = rows_per_instance * iid;
	int end = first + rows_per_instance;
	if (parameter_SIZE_Y() < end) {
		end = parameter_SIZE_Y();
	}
	return end - first;
}
