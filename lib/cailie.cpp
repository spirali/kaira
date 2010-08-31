
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "cailie.h"
#include "cailie_threads.h"
#include "cailie_internal.h"

CaContext::CaContext(int node, CaModule *module) 
{
	_node = node;
	_module = module;
	_halt_flag = false;
}

void CaContext::_init(int iid, int instances) 
{
	_iid = iid;
	_instances = instances;
}

static int ca_recv(CaContext *ctx, RecvFn *recv_fn, void *data)
{
	return ctx->_get_module()->recv(ctx, recv_fn, data);
}

void ca_start(CaContext *ctx, void *data, TransitionFn **wtransitions, RecvFn *recv_fn) {
	TransitionFn **wt = wtransitions + 1;
	TransitionFn **last_executed = wtransitions;
	for(;;) {
		if ((*wt) == NULL) {
			wt = wtransitions;
		}
		if (wt == last_executed) {
			if ((*wt)(ctx, data)) {
				if (ctx->_check_halt_flag()) {
					return;
				}
				ca_recv(ctx, recv_fn, data);
			} else {
				while(!ca_recv(ctx, recv_fn, data)) {}	
			}
		} else {
			if ((*wt)(ctx, data)) {
				ca_recv(ctx, recv_fn, data);
				last_executed = wt;
			}
		}
		wt++;
	}
}

void ca_main(int nodes_count, MainFn *main_fn) {
	CaModule *m = new CaThreadsModule();
	m->main(nodes_count, main_fn);
}

void ca_send(CaContext *ctx, int node, int data_id, void *data, size_t data_size)
{
	ctx->_get_module()->send(ctx, node, data_id, data, data_size);
}

static int ca_set_argument(int params_count, const char **param_names, int **param_data, char *name, char *value)
{
	int t;
	for (t = 0; t < params_count; t++) {
		if (!strcmp(name, param_names[t])) {
			char *err = NULL;
			int i = strtol(value, &err, 10);
			if (*err != '\0') {
				printf("Invalid parameter value\n");
				exit(1);
			}
			(*param_data[t]) = i;
			return t;
		}
	}
	printf("Unknown parameter '%s'\n", name);
	exit(1);
}

void ca_parse_args(int argc, char **argv, int params_count, const char **param_names, int **param_data, const char **param_descs)
{
	int t, c;
	struct option longopts[] = {
		{ "help",	0,	NULL, 'h' },
		{ NULL,		0,	NULL,  0}
	};

	bool setted[params_count];
	for (t = 0; t < params_count; t++) {
		setted[t] = false;
	}

	while ((c = getopt_long (argc, argv, "hp:", longopts, NULL)) != -1)
		switch (c) {
			case 'h': {
				int max_len = 0;
				for (t = 0; t < params_count; t++) {
					if (max_len > strlen(param_names[t])) {
						max_len = strlen(param_names[t]);
					}
				}
				for (t=0; t < params_count; t++) {
					printf("Parameters:\n");
					printf("%s", param_names[t]);
					for (int s = strlen(param_names[t]); s < max_len; s++) { printf(" "); }
					printf(" - %s\n", param_descs[t]);
				}
				exit(0);
			}
			case 'p': {
				char str[strlen(optarg) + 1];
				strcpy(str, optarg);
				char *s = str;
				while ( (*s) != 0 && (*s) != '=') { s++; }
				if ((*s) == 0) {
					printf("Invalid format of -p\n");
					exit(1);
				}
				*s = 0;
				s++;
				int r = ca_set_argument(params_count, param_names, param_data, str, s);
				setted[r] = true;
			} break;

			case '?':
			default:
				exit(1);
			}
	bool exit_f = false;
	for (t = 0; t < params_count; t++) {
		if (!setted[t]) {
			exit_f = true;
			printf("Mandatory parameter '%s' required\n", param_names[t]);
		}
	}
	if (exit_f) { exit(1); }
}
