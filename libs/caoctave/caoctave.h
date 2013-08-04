#ifndef CAOCTAVE_H
#define CAOCTAVE_H

#define CAOCTAVE 1

#include <octave/oct.h>

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

	/* To octave value */

	template<typename T> octave_value to_octave_value(const T &value) {
		return octave_value(value);
	}
}

#endif // CAOCTAVE_H
