#ifndef CAOCTAVE_H
#define CAOCTAVE_H

#define CAOCTAVE 1

#include <cailie.h>
#include <octave/oct.h>
#include <octave/load-save.h>
#include <octave/ov-re-mat.h>
#include <ov-re-sparse.h>
#include <ls-oct-binary.h>

namespace caoctave {

	/* From octave value */

	inline void from_octave_value(float &value, const octave_value &ovalue) {
		value = ovalue.float_value();
	}

	inline void from_octave_value(double &value, const octave_value &ovalue) {
		value = ovalue.double_value();
	}

	inline void from_octave_value(bool &value, const octave_value &ovalue) {
		value = ovalue.bool_value();
	}

	inline void from_octave_value(int &value, const octave_value &ovalue) {
		value = ovalue.int_value();
	}

	inline void from_octave_value(long &value, const octave_value &ovalue) {
		value = ovalue.long_value();
	}

	inline void from_octave_value(unsigned int &value, const octave_value &ovalue) {
		value = ovalue.uint_value();
	}

	inline void from_octave_value(unsigned long &value, const octave_value &ovalue) {
		value = ovalue.ulong_value();
	}

	inline void from_octave_value(std::string &value, const octave_value &ovalue) {
		value = ovalue.string_value();
	}

	inline void from_octave_value(Matrix &value, const octave_value &ovalue) {
		value = ovalue.matrix_value();
	}

	inline void from_octave_value(SparseMatrix &value, const octave_value &ovalue) {
		value = ovalue.sparse_matrix_value();
	}

	/* To octave value */

	template<typename T> octave_value to_octave_value(const T &value) {
		return octave_value(value);
	}
}

/* Packing of octave types */

namespace ca {

	CA_STREAM_TOKEN_NAME(Matrix)
	CA_STREAM_TOKEN_NAME(SparseMatrix)

	CA_PACK(Matrix, packer, m) {
		std::stringstream s;
		write_header(s, LS_BINARY);
		bool flag = false;
		octave_value(m).save_binary(s, flag);
		pack(packer, s.str());
	}

	CA_UNPACK(Matrix, unpacker, m) {
		std::string s;
		unpacker >> s;
		std::stringstream data(s);
		bool swap;
		std::string name, doc;
		oct_mach_info::float_format flt_fmt;
		read_binary_file_header(data, swap, flt_fmt);
		octave_matrix mv;
		mv.load_binary(data,swap,flt_fmt);
		m = mv.matrix_value();
	}

	CA_PACK(SparseMatrix, packer, m) {
		std::stringstream s;
		write_header(s, LS_BINARY);
		bool flag = false;
		octave_value(m).save_binary(s, flag);
		pack(packer, s.str());
	}

	CA_UNPACK(SparseMatrix, unpacker, m) {
		std::string s;
		unpacker >> s;
		std::stringstream data(s);
		bool swap;
		std::string name, doc;
		oct_mach_info::float_format flt_fmt;
		octave_sparse_matrix mv;
		read_binary_file_header(data, swap, flt_fmt);
		mv.load_binary(data,swap,flt_fmt);
		m = mv.sparse_matrix_value();
	}
}

#endif // CAOCTAVE_H
