#ifndef CAOCTAVE_H
#define CAOCTAVE_H

#define CAOCTAVE 1

#include <cailie.h>
#include <octave/oct.h>
#include <octave/load-save.h>
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

	inline void pack(Packer &packer, Matrix &m) {
		std::stringstream s;
		write_header(s, LS_BINARY);
		bool flag = false;
		octave_value(m).save_binary(s, flag);
		pack(packer, s.str());
	}

	template<> inline Matrix unpack<Matrix>(Unpacker &unpacker) {
		std::stringstream data(unpack<std::string>(unpacker));
		bool swap;
		std::string name, doc;
		oct_mach_info::float_format flt_fmt;
		octave_value ovalue = Matrix();
		read_binary_file_header(data, swap, flt_fmt);
		ovalue.load_binary(data,swap,flt_fmt);
		return ovalue.matrix_value();
	}

	inline void pack(Packer &packer, SparseMatrix &m) {
		std::stringstream s;
		write_header(s, LS_BINARY);
		bool flag = false;
		octave_value(m).save_binary(s, flag);
		pack(packer, s.str());
	}

	template<> inline SparseMatrix unpack<SparseMatrix>(Unpacker &unpacker) {
		std::stringstream data(unpack<std::string>(unpacker));
		bool swap;
		std::string name, doc;
		oct_mach_info::float_format flt_fmt;
		octave_value ovalue = SparseMatrix();
		read_binary_file_header(data, swap, flt_fmt);
		ovalue.load_binary(data,swap,flt_fmt);
		return ovalue.sparse_matrix_value();
	}
}

#endif // CAOCTAVE_H
