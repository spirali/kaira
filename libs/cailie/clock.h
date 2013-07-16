#ifndef CAILIE_CLOCK_H
#define CAILIE_CLOCK_H

#include <time.h>

namespace ca {
	typedef uint64_t IntTime;
	const IntTime MAX_INT_TIME = ~((IntTime)0);

	inline IntTime time_diff(
			const struct timespec &old_time,
			const struct timespec &new_time) {
		uint64_t t =
			((uint64_t) (new_time.tv_sec - old_time.tv_sec))
			* 1000000000;
		t += new_time.tv_nsec - old_time.tv_nsec;
		return t;
	}

	class Clock {
		public:
			virtual ~Clock() {}

			void tick() {
				if (clock_gettime(CLOCK_MONOTONIC, &time)) {
					perror("ca::Clock::tick");
					exit(-1);
				}
			}

			virtual IntTime tock() {
				struct timespec new_time;
				if (clock_gettime(CLOCK_MONOTONIC, &new_time)) {
					perror("ca::Clock::tock");
					exit(-1);
				}
				return time_diff(time, new_time);
			}

		protected:
			struct timespec time;
	};

}

#endif // CAILIE_CLOCK_H
