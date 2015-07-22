#include "vertices.h"

using namespace cass;

void HashVertex::assign() {
	if (processes.size()) {
		owner = processes[0];
		for (size_t i = 1; i < processes.size(); i++) {
			if (owner->nodeCount > processes[i]->nodeCount) {
				owner = processes[i];
			}
		}
		owner->assign(this);
		owner->realNodes++;
	}
}

void HashVertex::release(ProcessVertex *process)
{
	if (owner->redirectable()) {
		owner->release(this);
		owner = process;
	}
}



bool ProcessVertex::redirectable()
{
	if (assigned.size() > 1) {
		return true;
	}

	return false;
}

void ProcessVertex::clear() {
	hashes.clear();
	assigned.clear();
	next_size = 0;
}

void ProcessVertex::assign(HashVertex *vertex) {
	next_size += vertex->ample_size;
	nodeCount++;
	vertex->assigned++;
	assigned.insert(vertex);
}

void ProcessVertex::release(HashVertex *vertex) {
	next_size -= vertex->ample_size;
	nodeCount--;
	vertex->assigned--;
	assigned.erase(vertex);
}

void ProcessVertex::steal() {
	if (next_size || hashes.size() == 0) {
		return;
	}
	size_t i;
	int min = -1;
	for (i = 0; i < hashes.size(); i++) {
		if (hashes[i]->processes.size() && hashes[i]->owner->rank != rank) {
			min = i;
			break;
		}
	}
	if (min == -1) {
		return;
	}
	for (i = min + 1; i < hashes.size(); i++) {
		if (hashes[min]->assigned > hashes[i]->assigned) {
			min = i;
		}
	}
	assign(hashes[min]);
	hashes[min]->release(this);
};

void ProcessVertex::add_vertex(HashVertex* hash) {
	hashes.push_back(hash);
}


