
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <iomanip>

#define WIDTH 10
#define COLUMN_DATA_SIZE (sizeof(Place) * WIDTH)

struct Place {
	double foxes;
	double rabbits;
};

class Column {
	
	public:

	Column() {
		array = new Place[WIDTH];
	}

	Column(const Place *data) {
		array = new Place[WIDTH];
		set_raw_data(data);
	}

	~Column() {
		delete [] array;
	}

	Column(const Column &c) {
		array = new Place[WIDTH];
		set_raw_data(c.get_raw_data());
	}

	Column & operator= (const Column & c) {
        if (this != &c)
		{
			set_raw_data(c.get_raw_data());
        }
        return *this;
	}


	const Place * get_raw_data() const { return array; }
	void set_raw_data(const Place *data) {
		memcpy(array, data, COLUMN_DATA_SIZE);
	}

	public:
		Place *array;

};

class Data {
	public:
		Data(const int sizex) {
			size = sizex;
			array1 = new Place[sizex * (WIDTH + 2)];
			array2 = new Place[sizex * (WIDTH + 2)];
		}
		
		void random_fill() {
			int t;
			for (t = WIDTH; t < (size + 1) * WIDTH; t++) {
				array1[t].rabbits = rand() % 100;
				array1[t].foxes = rand() % 20;
			}
		}
		
		void compute(Column &c1, Column &c2)
		{
			memcpy(array1, c1.get_raw_data(), sizeof(Place) * WIDTH);
			memcpy(array1 + (size + 1) * WIDTH, c2.get_raw_data(), sizeof(Place) * WIDTH);
			int t, s;
			for (t = WIDTH; t < (size + 1) * WIDTH; t+=WIDTH) {
				for (s = 0; s < WIDTH; s++) {
					int i = t + s;
					int u = i - WIDTH;
					int d = i + WIDTH; 
					int l = t + ((s + WIDTH - 1) % WIDTH);
					int r = t + ((s + 1) % WIDTH);
					double fsum = 
						array1[u].foxes + array1[d].foxes + array1[l].foxes + array1[r].foxes;
					double rsum = 
						array1[u].rabbits + array1[d].rabbits + array1[l].rabbits + array1[r].rabbits;
					compute_place(array1[i], array2[i], fsum, rsum);
				}
			}
			Place *a = array1;
			array1 = array2;
			array2 = a;
			get_columns(c1, c2);
		}

		void get_columns(Column &c1, Column &c2) 
		{
			c1.set_raw_data(array1 + WIDTH);
			c2.set_raw_data(array1 + size * WIDTH);
		}

		inline void compute_place(Place &place, Place &out, const double fsum, const double rsum)
		{
			out.rabbits = 2 * place.rabbits - 5 * place.foxes + 0.2 * rsum;
			out.foxes = 0.2 * place.foxes - 0.2 * (place.rabbits - 5 * place.foxes) + 0.1 * fsum;

			if (out.rabbits < 0.1) { out.rabbits = 0.0; }
			if (out.foxes < 0.1) { out.foxes = 0.0; }			
			
			if (out.rabbits > 500.0) { out.rabbits = 500; }
			if (out.foxes > 250.0) { out.foxes = 250.0; }			
			
		}
		
		std::string as_html() {
			std::stringstream stream;
			stream.precision(2);
			stream.setf(std::ios_base::fixed);
			for (int t = WIDTH; t < (size + 1) * WIDTH; t+=WIDTH) {
				stream << "<tr>" << std::endl;
				for (int s = 0; s < WIDTH; s++) {
					int i = t + s;
					stream << "<td>" << array1[i].rabbits << "<br/>" << array1[i].foxes << "<td/>" << std::endl;
				}
				stream << "</tr>" << std::endl;
			}
			return stream.str();
		}

		~Data() {
			delete [] array1;
			delete [] array2;
		}

	protected:
		int size;
		Place *array1, *array2;
};


