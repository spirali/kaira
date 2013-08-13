#ifndef CAVERIF_VERIFCONFIGURATION_H
#define CAVERIF_VERIFCONFIGURATION_H

namespace cass {

class Arc;

class VerifConfiguration {
	public:
		virtual bool compare(const Arc &arc1,const Arc &arc2) = 0;
		virtual bool is_transition_analyzed(int transition_id) = 0;
		virtual ~VerifConfiguration() {};

};

}

#endif // CAVERIF_VERIFCONFIGURATION_H
