
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>

#include "place.h"
#include "unit.h"
#include "process.h"
#include "output.h"

class CaContext {

};

/* Start */
void ca_main(int defs_count, CaUnitDef **defs);

/* Others */
void ca_parse_args(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_project_description(const char *str);

#endif
