#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>
#include <fstream>
#include <string.h>
#include <vector>

class Matrix
{
	public:
		Matrix() {
			this->_rows = 0;
			this->_columns = 0;
		}

		Matrix(int rows, int columns)
		{
			this->_rows = rows;
			this->_columns = columns;
			this->_data.resize(rows * columns, 0);
		}

		friend std::ostream& operator<<(std::ostream& os, const Matrix &m);

        Matrix operator*(const Matrix &other)
        {
            Matrix result(this->_rows, other._columns);
            for (int i = 0; i < this->_rows; i++) {
                for (int j = 0; j < other._columns; j++) {
                    for (int k = 0; k < this->_columns; k++) {
                        result(i, j) += (*this)(i, k) * other(k, j);
                    }
                }
            }
            return result;
        }

        Matrix& operator+=(const Matrix &other)
        {
            for (int i = 0; i < this->_rows; i++) {
                for (int j = 0; j < this->_columns; j++) {
                    (*this)(i, j) += other(i, j);
                }
            }
            return *this;
        }

        Matrix submatrix(int r, int c, int rc, int cc)
        {
            Matrix sub(rc, cc);
            for (int i = 0; i < rc; i++) {
                for (int j = 0; j < cc; j++) {
                    sub(i, j) = (*this)(i + r, j + c);
                }
            }
            return sub;
        }

        void insert(const Matrix &other, int r, int c)
        {
            for (int i = 0; i < other.rows(); i++) {
                for (int j = 0; j < other.columns(); j++) {
                    (*this)(i + r, j + c) = other(i, j);
                }
            }
        }

        double& operator()(int row, int column)
        {
            return this->_data[row * _columns + column];
        }

        const double& operator()(int row, int column) const
        {
            return this->_data[row * _columns + column];
        }

		const std::vector<double>& data() const
		{
			return _data;
		}

		std::vector<double>& data()
		{
			return _data;
		}

		int rows() const {
			return _rows;
		}

		int columns() const {
			return _columns;
		}

	protected:
		std::vector<double> _data;
		int _rows;
		int _columns;
};

std::ostream& operator<<(std::ostream& os, const Matrix &m)
{
	for(int i = 0; i < m.rows(); i++) {
		for(int j = 0; j < m.columns(); j++) {
			os << m(i, j) << " ";
		}
		os << std::endl;
	}
	os << std::endl;
	return os;
}


#endif // MATRIX_H
