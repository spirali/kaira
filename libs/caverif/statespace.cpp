
#include "statespace.h"
#include <vector>

extern CaNetDef **defs;
extern int defs_count;
extern int ca_process_count;

using namespace cass;

Node::Node(CaNetDef *net_def, CaThreadBase *thread)
{
	nets = new Net*[ca_process_count];
	for (int i = 0; i < ca_process_count; i++) {
		nets[i] = (Net*) net_def->spawn(thread);
	}
}

Node::Node()
{
	nets = new Net*[ca_process_count];
}

Node::Node(const std::vector<TransitionActivation> & activations)
	: activations(activations)
{
	nets = new Net*[ca_process_count];
}

Node::~Node()
{
	delete [] nets;
}

void Node::generate(Core *core)
{
	CaNetDef *def = core->get_net_def();
	const std::vector<CaTransitionDef*> &transitions =
		def->get_transition_defs();
	int transitions_count = def->get_transitions_count();

	for (int p = 0; p < ca_process_count; p++) {
		Node *node = NULL;
		for (int i = 0; i < transitions_count; i++) {
			if (node == NULL) {
				node = copy();
			}
			Net *net = node->nets[p];
			void *data = transitions[i]->fire_phase1(core->get_thread(), net);
			if (data) {
				TransitionActivation activation;
				activation.data = data;
				activation.transition_def = transitions[i];
				activation.process_id = p;
				activation.thread_id = 1;
				node->activations.push_back(activation);
				Node *n = core->add_node(node);
				nexts.push_back(n);
				node = NULL;
			}
		}
	}
	std::vector<TransitionActivation>::iterator i;
	int p = 0;
	for (i = activations.begin(); i != activations.end(); i++, p++)
	{
		Node *node = copy();
		node->activations.erase(node->activations.begin() + p);
		Net *net = node->nets[i->process_id];
		i->transition_def->fire_phase2(core->get_thread(), net, i->data);
		Node *n = core->add_node(node);
		nexts.push_back(n);
	}
}

Node* Node::copy()
{
	Node *node = new Node(activations);
	for (int i = 0; i < ca_process_count; i++) {
		node->nets[i] = nets[i]->copy();
	}
	return node;
}

bool Node::state_equals(const Node &node) const
{
	if (activations.size() != node.activations.size()) {
		return false;
	}
	std::vector<TransitionActivation>::const_iterator i1;
	std::vector<TransitionActivation>::const_iterator i2;
	for (i1 = node.activations.begin(); i1 != node.activations.end(); i1++) {
		for (i2 = activations.begin(); i2 != activations.end(); i2++) {
			if (i1->transition_def == i2->transition_def &&
				i1->process_id == i2->process_id &&
				i1->thread_id == i2->thread_id &&
				i1->transition_def->binding_equality(i1->data, i2->data))
			{
				break;
			}
		}
		if (i2 == activations.end())
			return false;
	}

	for (int i = 0; i < ca_process_count; i++) {
		if (!nets[i]->is_equal(*node.nets[i]))
			return false;
	}
	return true;
}

Core::Core() : initial_node(NULL)
{
}

Core::~Core()
{
}

void Core::generate()
{
	net_def = defs[0]; // Take first definition
	initial_node = new Node(net_def, &thread);
	add_node(initial_node);
	do {
		Node *node = not_processed.top();
		not_processed.pop();
		node->generate(this);
	} while (!not_processed.empty());
}

void Core::verify()
{
	printf("digraph X {\n");
	google::sparse_hash_set<Node*,
							NodeStateHash,
							NodeStateEq>::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = *it;
		const std::vector<Node*> &nexts = node->get_nexts();
		for (size_t i = 0; i < nexts.size(); i++) {
			printf("\"%p/%u\" -> \"%p/%u\"\n",
				node, node->get_activations().size(),
				nexts[i], nexts[i]->get_activations().size());
		}
	}
	printf("}\n");
}

Node * Core::add_node(Node *node)
{
	Node *n = get_node(node);
	if (n == NULL) {
		nodes.insert(node);
		not_processed.push(node);
		return node;
	} else {
		delete node;
		return n;
	}
}


bool Core::is_known_node(Node *node) const
{
	return nodes.find(node) != nodes.end();
}

Node * Core::get_node(Node *node) const
{
	google::sparse_hash_set<Node*,
							NodeStateHash,
							NodeStateEq>::const_iterator it;
	it = nodes.find(node);
	if (it == nodes.end())
		return NULL;
	else
		return *it;
}
