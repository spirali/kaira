
#include "cailie.h"

using namespace ca;

namespace ca {
extern Process **processes;
}

void ca::Process::broadcast_packet(int tag, void *data, size_t size, Thread *thread, int exclude)
{
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		void *d = malloc(size);
		memcpy(d, data, size);
		processes[t]->add_packet(process_id, tag, d);
	}
	free(data);
}

void ca::Process::add_packet(int from_process, int tag, void *data)
{
	Packet *packet = new Packet;
	packet->from_process = from_process;
	packet->tag = tag;
	packet->data = data;
	packet->next = NULL;
	pthread_mutex_lock(&packet_mutex);
	if (packets == NULL) {
		packets = packet;
	} else {
		Packet *p = packets;
		while (p->next) {
			p = p->next;
		}
		p->next = packet;
	}
	pthread_mutex_unlock(&packet_mutex);
}

void ca::Process::multisend_multicast(
	const std::vector<int> &targets,
	Net *net,
	int place_index,
	int tokens_count,
	const Packer &packer,
	Thread *thread)
{
	std::vector<int>::const_iterator i;
	Tokens *data = (Tokens*) packer.get_buffer();
	data->place_index = place_index;
	data->tokens_count = tokens_count;
	data->msg_id = thread->get_msg_id();
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i;
		if(target < 0 || target >= process_count) {
			fprintf(stderr,
					"Net sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
					tokens_count, target, process_count - 1);
			exit(1);
		}
		CA_DLOG("SEND index=%i target=%i process=%i\n", place_index, target, get_process_id());
		void *d;
		if ((i + 1) == targets.end()) {
			d = data;
		} else {
			d = malloc(packer.get_size());
			memcpy(d, data, packer.get_size());
		}
		Process *p = processes[target];
		p->add_packet(process_id, CA_TAG_TOKENS, d);

	}
}

int ca::Process::process_packets(Thread *thread)
{
	if (packets) {
		pthread_mutex_lock(&packet_mutex);
		Packet *p = packets;
		packets = NULL;
		pthread_mutex_unlock(&packet_mutex);

		/* Now we have to be sure that all thread messages
           are processed and we know about all nets */
		thread->process_thread_messages();

		while (p) {
			process_packet(thread, p->from_process, p->tag, p->data);
			Packet *next = p->next;
			delete p;
			p = next;
		}
		return 1;
	}
	return 0;
}
