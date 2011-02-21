
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <string>
#include <stack>
#include <vector>
#include <string.h>

class CaContext;
class CaOutput;

/* Function types */
typedef int(TransitionFn)(CaContext * ctx, void *places);
typedef void(InitFn)(CaContext * ctx);
typedef void(RecvFn)(void *places,  int data_id, void *data, size_t size);
typedef void(ReportFn)(CaContext * ctx, void *data, CaOutput *out);
typedef int(NodeToProcessFn)(int node);

class CaModule;
class CaProcess;
class CaTransition;
class CaJob;

class CaContext {

	public:
		CaContext(int node, CaProcess* process);

		/* CaContext user API */
		int node() { return _node; }
		int iid() { return _iid; }
		int instances() { return _instances; }
		void halt();
		void quit();

		/* Internal */
		size_t _get_reserved_prefix_size();
		CaProcess * _get_process() { return _process; }
		void _init(int iid, int instances, void * places, RecvFn *recv_fn, ReportFn *report_fn);
		void _register_transition(int id, TransitionFn *fn);
		int _check_halt_flag() { return _halt_flag; }
		void * _get_places() { return _places; }
		RecvFn * _get_recv_fn() { return _recv_fn; }
		void _call_recv_fn(int data_id, void *data, size_t size) { return _recv_fn(_places, data_id, data, size); }
		std::vector<CaTransition> & _get_transitions() { return _transitions; }
		bool _find_transition(int id, CaTransition &transition);
		ReportFn * _get_report_fn() { return _report_fn; }
		CaJob * _get_jobs();
	protected:
		int _node;
		int _iid;
		int _instances;
		bool _halt_flag;
		CaProcess *_process;
		RecvFn *_recv_fn;
		void *_places;
		std::vector<CaTransition> _transitions;
		ReportFn * _report_fn;
};

class CaOutputBlock;

class CaOutput {
	public:
		~CaOutput();

		void child(const std::string &name);
		CaOutputBlock * back();
		void set(const std::string &name, const int i);
		void set(const std::string &name, const std::string &s);
	private:
		void _set(const std::string &name, const std::string &s);
		std::stack<CaOutputBlock*> _stack;
};

class CaTransition {
	public:
		CaTransition() {}
		CaTransition(int id, TransitionFn *fn) { this->id = id; this->function = fn; }
		int call(CaContext *ctx) const { return function(ctx, ctx->_get_places()); }
		int get_id() const { return id; }
	protected:
		TransitionFn *function;
		int id;
};

class CaPacker {

	public:
		CaPacker(size_t size);
		CaPacker(size_t size, size_t reserved);
		void pack(const void *mem, size_t size) { memcpy(buffer_pos, mem, size); buffer_pos += size;  }
		void pack_size(size_t data) { pack(&data, sizeof(size_t)); }
		void pack_string(std::string str) { size_t s = str.size(); pack_size(s); pack(str.c_str(), s); }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
		size_t get_size() { return size; }
		void * get_buffer() { return buffer; }
	protected:
		char *buffer_pos;
		size_t size;
		char *buffer;
};

class CaUnpacker {

	public:
		CaUnpacker() {}
		CaUnpacker(void *mem) { buffer_pos = (char*) mem; }
		void * unpack(size_t size) { void *p = buffer_pos; buffer_pos += size; return p; }
		void unpack(void *data, size_t size) { memcpy(data, buffer_pos, size); buffer_pos += size; }
		size_t unpack_size() { size_t *data = (size_t*)unpack(sizeof(size_t)); return *data; }
		std::string unpack_string() { size_t s = unpack_size(); return std::string((char*) unpack(s), s); }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
	protected:
		char *buffer_pos;
};

template<class T> class CaPlace {
	public:
		CaPlace() {}

		void add(T element) { list.push_back(element); }
		void remove_at(int pos) { list.erase(list.begin() + pos); }
		void clear() { list.clear(); }
		size_t size() { return list.size(); }
		T operator[] (size_t pos) { return list[pos]; }
		std::vector<T> as_vector() { return list; }

	protected:
		/* This is naive implementation, it needs benchmarks
			to choose correct implementation */
		std::vector<T> list;

};

/* Init functions */
void ca_main(int nodes_count, InitFn *init_fn);
void ca_set_node_to_process(NodeToProcessFn *fn);

/* Communications */
void ca_send(CaContext *ctx, int node, int data_id, CaPacker &packer);

/* Others */
void ca_parse_args(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);

/* Helper utils */
std::string ca_int_to_string(int i);
std::string ca_double_to_string(double d);
std::string ca_float_to_string(float f);

/* Global variables */
extern int ca_process_count;

#endif
