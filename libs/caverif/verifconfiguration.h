#ifndef CAVERIF_VERIFCONFIGURATION_H
#define CAVERIF_VERIFCONFIGURATION_H

#include <packing.h>

namespace cass {

class Arc;

class VerifConfiguration {
	public:
		virtual bool compare(const Arc &arc1,const Arc &arc2) = 0;
		virtual bool is_transition_analyzed(int transition_id) = 0;
		virtual void pack_final_marking(ca::NetBase *net, ca::Packer &packer) = 0;
		virtual ~VerifConfiguration() {};

};

}

#endif // CAVERIF_VERIFCONFIGURATION_H
