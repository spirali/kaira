
#include "cailie.h"

extern CaProcess **processes;

void CaProcess::broadcast_packet(int tag, void *data, size_t size, int exclude)
{
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		void *d = malloc(size);
		memcpy(d, data, size);
		processes[t]->add_packet(tag, d);
	}
}

void CaProcess::add_packet(int tag, void *data)
{
	CaPacket *packet = new CaPacket;
	packet->tag = tag;
	packet->data = data;
	packet->next = NULL;
	pthread_mutex_lock(&packet_mutex);
	if (packets == NULL) {
		packets = packet;
	} else {
		CaPacket *p = packets;
		while (p->next) {
			p = p->next;
		}
		p->next = packet;
	}
	pthread_mutex_unlock(&packet_mutex);
}

void CaProcess::multisend_multicast(const std::vector<int> &targets, CaNet *net, int place_index, int tokens_count, const CaPacker &packer)
{
	char *buffer = packer.get_buffer();
	std::vector<int>::const_iterator i;
	CaTokens *data = (CaTokens*) packer.get_buffer();
	data->place_index = place_index;
	data->net_id = net->get_id();
	data->tokens_count = tokens_count;
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i % process_count;
		CA_DLOG("SEND index=%i target=%i process=%i\n", place_index, target, get_process_id());
		CaProcess *p = processes[target];
		void *d = malloc(packer.get_size());
		memcpy(d, data, packer.get_size());
		p->add_packet(CA_TAG_TOKENS, d);

	}
	free(buffer);
}

int CaProcess::process_packets(CaThread *thread)
{
	if (packets) {
		pthread_mutex_lock(&packet_mutex);
		CaPacket *p = packets;
		packets = NULL;
		pthread_mutex_unlock(&packet_mutex);

		/* Now we have to be sure that all thread messages
           are processed and we know about all nets */
		thread->process_thread_messages();

		while (p) {
			process_packet(thread, p->tag, p->data);
			CaPacket *next = p->next;
			free(p->data);
			delete p;
			p = next;
		}
		return 1;
	}
	return 0;
}
