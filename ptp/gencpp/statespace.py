#
#    Copyright (C) 2012-2014 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
#


import build
import buildnet
import base.utils as utils

def switch_by_id(builder, expr, items, callback, exclude=None):
    builder.switch_begin(expr)
    for i in items:
        if i == exclude:
            continue
        builder.line("case {0.id}:", i)
        builder.block_begin()
        callback(builder, i)
        builder.line("break;")
        builder.block_end()
        builder.emptyline()
    builder.line("default:")
    builder.line("fprintf(stderr, \"Internal error\");")
    builder.line("abort();")
    builder.block_end()

def write_dependent(builder):
    net = builder.project.nets[0]

    def write_fire_fire(t1, t2):
        def get_place(edge):
            return edge.place

        # Transitions have different priority -> they can disable each other
        if t1.priority != t2.priority:
            builder.line("return true; // Transitions have different priority")
            return

        # Both transitions send tokens but we don't know exactly where
        if not t1.is_local() and not t2.is_local() and \
                not (t1.has_fixed_target() and t2.has_fixed_target()):
            builder.line("return true; "
                         "// Both transitions send tokens but we don't know exactly where")
            return

        # Transitions taking tokens from the same input place
        if t1.get_input_places().intersection(t2.get_input_places()):
            builder.line("return true; "
                         "// Transitions taking tokens from the same input place")
            return

        # A transition putting tokens to a input place of a bulk transition
        edges1 = [ e2 for e1, e2 in utils.objects_with_same_attribute(
                                        t1.edges_in, t2.edges_out, get_place)
                      if e1.is_bulk() ]

        edges2 = [ e2 for e1, e2 in utils.objects_with_same_attribute(
                                        t2.edges_in, t1.edges_out, get_place)
                   if e1.is_bulk() ]

        for e in edges1 + edges2:
            builder.line("// A transition putting tokens to a input place of a bulk transition")
            if e.has_fixed_target() and all(not i.is_local() for i in e.inscriptions):
                condition = " || ".join("a1.process == ({0})".format(i.target)
                                        for i in e.inscriptions)
                builder.line("if ({0}) return true;", condition)
            else:
                builder.line("return true;", t2)
                return

        # Transitions putting tokens to the same local place
        for e1, e2 in utils.objects_with_same_attribute(t1.edges_out, t2.edges_out, get_place):
            if e1.has_fixed_target() and e2.has_fixed_target():
                if all(not i.is_local() for i in e1.inscriptions):
                    condition1 = " || ".join("(a1.process == ({0}))".format(i.target)
                                             for i in e1.inscriptions)
                else:
                    condition1 = "true"
                if all(not i.is_local() for i in e2.inscriptions):
                    condition2 = " || ".join("(a1.process == ({0}))".format(i.target)
                                             for i in e2.inscriptions)
                else:
                    condition2 = "true"
                builder.line("// Transitions putting tokens to the same local place")
                builder.line("if (({0}) && ({1})) return true;", condition1, condition2)

        # Transitions sending tokens to the same process
        edges1 = [ e for e in t1.edges_out if e.has_fixed_target() and not e.is_local() ]
        edges2 = [ e for e in t2.edges_out if e.has_fixed_target() and not e.is_local() ]
        for e1 in edges1:
            for e2 in edges2:
                targets1 = [i.target for i in e1.inscriptions if i.target is not None]
                targets2 = [i.target for i in e2.inscriptions if i.target is not None]
                builder.line("// Transitions sending tokens to the same process")
                builder.line("if ({0}) return true;",
                             " || ".join("({0}) == ({1})".format(t1, t2)
                                    for t1 in targets1 for t2 in targets2))
        builder.line("return false;")

    def write_fire_receive(fire, receive):
        def fire_callback(builder, transition):
            # Receive token to the transition output place
            for t_edge in transition.edges_out:
                for r_edge in t_edge.place.get_edges_in():
                    builder.line("// t {0.transition.id} --> p {0.place.id}", r_edge)
                    builder.if_begin("a{1}.data.receive.edge_id == {0.id}", r_edge, receive)
                    targets = [ i.target for i in t_edge.inscriptions ]
                    if t_edge.has_fixed_target() and None not in targets:
                        builder.if_begin(" && ".join("a1.process != ({0})".format(t)
                                         for t in targets))
                        builder.line("return false;")
                        builder.block_end()
                    builder.line("return true;")
                    builder.block_end()

            # Receive token to a place with transition's input place with bulk edge
            for t_edge in transition.get_bulk_edges_in():
                for r_edge in t_edge.place.get_edges_in():
                    builder.line("// t {0.transition.id} --> p {0.place.id}", r_edge)
                    builder.line("if (a{1}.data.receive.edge_id == {0.id}) return true;",
                                 r_edge, receive)
            builder.line("return false;")

        def receive_callback(builder, edge):
            builder.line("// t {0.transition.id} --> p {0.place.id}", edge)
            # Receive a token disable active transition
            #FIX ME: It not works for ample sets with more than one action
            builder.line("int edge_priority = 0;")
            for t in sorted(edge.place.get_transitions_out(), key = lambda t: t.priority):
                builder.line("if (is_enabled({0.id}, a1.process, marking, {2.id})) edge_priority = {1};",
                             t, t.priority, edge.place)
            for t in net.transitions:
                builder.if_begin("edge_priority > {2} && {0.id} == a{1}.data.fire.transition_def->get_id()",
                                 t, fire, t.priority)
                builder.line("return true;")
                builder.block_end()

        switch_by_id(builder,
                     "a{0}.data.receive.edge_id".format(receive),
                     net.get_edges_out(),
                     receive_callback)
        switch_by_id(builder,
                     "a{0}.data.fire.transition_def->get_id()".format(fire),
                     net.transitions,
                     fire_callback)

    def write_receive_receive():
        edges = [ edge for edge in net.get_edges_out() if not edge.is_local() ]
        builder.switch_begin("a1.data.receive.edge_id")
        for edge in edges:
            if not edge.is_local():
                builder.line("case {0.id}: // t {0.transition.id} --> p {0.place.id}", edge)
                for e in edge.place.get_edges_in():
                    if e is not edge and not e.is_local():
                        builder.line("if (a2.data.receive.edge_id == {0.id}) return true;", e)
                builder.line("return false;")
        builder.line("default: return false;")
        builder.block_end()

    builder.line("bool is_dependent("
                 "const cass::Action &a1, const cass::Action &a2, const std::vector<int> &marking)")
    builder.block_begin()
    builder.if_begin("a1.process != a2.process")
    builder.line("return false;")
    builder.block_end()
    builder.line("cass::VerifThread thread(a1.process);")
    builder.line("ca::Context ctx(&thread, NULL);");

    builder.if_begin("a1.type == cass::ActionFire && a2.type == cass::ActionFire")

    builder.if_begin("a1.data.fire.transition_def->get_id() == "
                     "a2.data.fire.transition_def->get_id()")
    builder.line("fprintf(stderr, \"Internal error - The same transitions in POR\");")
    builder.line("abort();")
    builder.block_end()

    switch_by_id(builder,
                 "a1.data.fire.transition_def->get_id()",
                 net.transitions,
                 lambda builder, t1:
                     switch_by_id(builder,
                         "a2.data.fire.transition_def->get_id()",
                         net.transitions,
                         lambda builder, t2: write_fire_fire(t1, t2),
                         exclude=t1))
    builder.block_end()

    builder.if_begin("a1.type == cass::ActionFire && a2.type == cass::ActionReceive")
    write_fire_receive(1, 2)
    builder.block_end()

    builder.if_begin("a1.type == cass::ActionReceive && a2.type == cass::ActionFire")
    write_fire_receive(2, 1)
    builder.block_end()

    builder.if_begin("a1.type == cass::ActionReceive && a2.type == cass::ActionReceive")

    builder.if_begin("a1.data.receive.edge_id == a2.data.receive.edge_id &&"
                     "a1.data.receive.source == a2.data.receive.source")
    builder.line("fprintf(stderr, \"Internal error - The same receive actions in POR\");")
    builder.line("abort();")
    builder.block_end()

    write_receive_receive()
    builder.block_end()
    builder.block_end()

def write_is_visible(builder):
    builder.line("bool is_visible(const cass::Action &action)")
    builder.block_begin()
    builder.if_begin("action.type == cass::ActionFire")
    for tr in builder.project.nets[0].transitions:
        if tr.calls_quit:
            builder.if_begin("action.data.fire.transition_def == &transition_{0.id}", tr)
            builder.line("return true;")
            builder.block_end()
    builder.block_end()
    builder.line("return false;")
    builder.block_end()

def write_compare_function(builder, compared):
    builder.line("bool compare(const cass::Arc &arc1, const cass::Arc &arc2)")
    builder.block_begin()
    builder.if_begin("arc1.nni->data.fire.transition_id == arc2.nni->data.fire.transition_id")
    builder.switch_begin("arc1.nni->data.fire.transition_id")
    for tr in compared:
        builder.line("case {0}:", tr.id)
        builder.block_begin()
        if tr.occurrence_analysis_compare_process and tr.occurrence_analysis_compare_binding:
                builder.if_begin("arc1.nni->data.fire.process_id == "
                                 "arc2.nni->data.fire.process_id")
                builder.line("return memcmp(" +
                         "arc1.nni->data.fire.binding, arc2.nni->data.fire.binding, "
                         "mhash_get_block_size(MHASH_MD5)) < 0;")
                builder.write_else()
                builder.line("return arc1.nni->data.fire.process_id < "
                                    "arc2.nni->data.fire.process_id;")
                builder.block_end()
        elif tr.occurrence_analysis_compare_process and not tr.occurrence_analysis_compare_binding:
                builder.line("return arc1.nni->data.fire.process_id < "
                                    "arc2.nni->data.fire.process_id;")
        elif not tr.occurrence_analysis_compare_process and tr.occurrence_analysis_compare_binding:
            builder.line("return memcmp(" +
                    "arc1.nni->data.fire.binding, arc2.nni->data.fire.binding, "
                    "mhash_get_block_size(MHASH_MD5)) < 0;")
        else:
            builder.line("return false;")
        builder.block_end()
    builder.block_end()
    builder.write_else()
    builder.line("return arc1.nni->data.fire.transition_id < "
                        "arc2.nni->data.fire.transition_id;")
    builder.block_end()
    builder.block_end()

def write_constructor(builder, ignored):
    builder.line("VerifConfiguration()")
    builder.block_begin()
    if ignored:
        builder.line("int transitions[] = {{{0}}};", ", ".join(str(i.id) for i in ignored))
        builder.line("for (int i = 0; i < {0}; i++)", len(ignored))
        builder.block_begin()
        builder.line("ignored_transitions.insert(transitions[i]);")
        builder.block_end()
    builder.block_end();

def write_compute_successors(builder):

    def push_fire(transition):
        builder.if_begin("is_enabled({0.id}, a.process, marking, -1)", transition)
        builder.line("action.type = cass::ActionFire;")
        builder.line("action.process = a.process;")
        builder.line("action.data.fire.transition_def = &transition_{0.id};", transition)
        builder.if_begin("enabled_priorities[action.process] <= transition_{0.id}.get_priority() && "
                         "processed.find(action) == processed.end()", transition)
        builder.line("queue.push_back(action);")
        builder.line("processed.insert(action);")
        builder.block_end()
        builder.block_end()

    def push_receive(edge, process):
        builder.line("action.type = cass::ActionReceive;")
        builder.line("action.process = {0};", process)
        builder.line("action.data.receive.source = a.process;")
        builder.line("action.data.receive.edge_id = {0.id};", edge)
        builder.if_begin("!receive_blocked[action.process * ca::process_count + a.process] && "
                         "processed.find(action) == processed.end()")
        builder.line("queue.push_back(action);")
        builder.line("processed.insert(action);")
        builder.block_end()

    def push_tokens(net, place):
        builder.line("// there is {0} places, place {1.id} is on position {2}.",
                     len(net.places), place, place.get_pos_id())
        builder.line("marking[{0} * a.process + {1}]++;", len(net.places), place.get_pos_id())

    def push_transitions_of_place(place, ignore_transition=None):
        for edge in place.get_edges_out():
            if edge.transition != ignore_transition:
                push_fire(edge.transition)

    def sucessors_of_transition(transition):
        for edge in transition.edges_out:
            builder.line("// place {0.place.id}", edge)
            if edge.is_local():
                push_tokens(net, edge.place)
                push_transitions_of_place(edge.place, transition)
            elif edge.has_fixed_target():
                for i in edge.inscriptions:
                    if i.is_local():
                        push_tokens(net, edge.place)
                        push_transitions_of_place(edge.place, transition)
                    else:
                        builder.block_begin()
                        builder.line("int $target = {0.target};", i)
                        builder.if_begin("a.process == $target")
                        push_tokens(net, edge.place)
                        push_transitions_of_place(edge.place, transition)
                        builder.write_else()
                        push_receive(edge, builder.expand("$target"))
                        builder.block_end()
                        builder.block_end()
            else:
                multicast = any(i.is_multicast() for i in edge.inscriptions)
                push_tokens(net, edge.place)
                push_transitions_of_place(edge.place, transition)
                builder.line("for (int $p = 0; $p < ca::process_count; $p++)")
                builder.block_begin()
                if not multicast:
                    builder.if_begin("$p != a.process")
                push_receive(edge, builder.expand("$p"))
                if not multicast:
                    builder.block_end()
                builder.block_end()
        builder.line("return;")

    net = builder.project.nets[0]
    builder.line("void compute_successors"
                    "(const cass::Action &a,"
                    "std::deque<cass::Action> &queue,"
                    "cass::ActionSet &processed,"
                    "const std::vector<bool> &receive_blocked,"
                    "const std::vector<int> &enabled_priorities,"
                    "std::vector<int> &marking)")

    builder.block_begin()
    builder.line("cass::Action action;")
    builder.line("cass::VerifThread thread(a.process);")
    builder.line("ca::Context ctx(&thread, NULL);");

    builder.switch_begin("a.type")
    builder.line("case cass::ActionFire:")
    switch_by_id(builder,
                 "a.data.fire.transition_def->get_id()",
                 net.transitions,
                 lambda builder, transition: sucessors_of_transition(transition))

    builder.line("case cass::ActionReceive:")
    builder.block_begin()
    builder.switch_begin("a.data.receive.edge_id")
    for net in builder.project.nets:
        for edge in net.get_edges_out():
            if edge.is_local():
                continue
            builder.line("case {0.id}: // t {0.transition.id} --> p {0.place.id}", edge)
            builder.block_begin()
            push_tokens(net, edge.place)
            push_transitions_of_place(edge.place)
            builder.line("return;")
            builder.block_end()
    builder.block_end()
    builder.line("return;")
    builder.block_end()
    builder.block_end()
    builder.block_end()

def write_get_marking(builder):
    net = builder.project.nets[0]
    builder.write_method_start("std::vector<int> get_marking(cass::State *s)")
    builder.line("std::vector<int> marking;")
    builder.line("marking.resize({0} * ca::process_count);", len(net.places))
    builder.for_begin("int p = 0; p < ca::process_count; p++")
    builder.line("Net_{0} *n = (Net_{0} *) s->get_net(p);", net.id)
    for i, p in enumerate(net.places):
        builder.line("marking[p * {0} + {2}] = n->get_token_count_in_place({1.id});",
                     len(net.places), p, i)
    builder.block_end()
    builder.line("return marking;")
    builder.block_end()

def write_is_enabled(builder):
    net = builder.project.nets[0]

    def callback(builder, transition):
        for p in transition.get_input_places():
            builder.line("if (marking[{0} * process_id + {2}] == 0 && {1.id} != ignored_place) return false;",
                         len(net.places), p, p.get_pos_id())

    builder.write_method_start("bool is_enabled("
                               "int transition_id, int process_id, const std::vector<int> &marking, int ignored_place)")
    switch_by_id(builder,
                 "transition_id",
                 net.transitions,
                 callback)
    builder.line("return true;")
    builder.block_end()

def write_verif_configuration(builder):
    ignored = []
    compared = []
    for net in builder.project.nets:
        compared += [ t for t in net.transitions if t.occurrence_analysis ]
        ignored += [ t for t in net.transitions if not t.occurrence_analysis ]

    builder.line("class VerifConfiguration : public cass::VerifConfiguration {{")
    builder.line("public:")

    write_constructor(builder, ignored)
    write_dependent(builder)
    write_is_visible(builder)
    write_compare_function(builder, compared)
    write_compute_successors(builder)
    write_get_marking(builder)
    write_is_enabled(builder)

    # Final Marking
    net = builder.project.nets[0]
    builder.line("void pack_final_marking(ca::NetBase *net, ca::Packer &packer)")
    builder.block_begin()
    builder.line("Net_{0} *n = (Net_{0} *) net;", net.id)
    for place in net.places:
        if place.final_marking:
            builder.line("ca::pack(packer, n->place_{0.id});", place)
    builder.block_end()
    builder.write_class_end()

def write_core(builder):
    build.write_basic_definitions(builder)
    for net in builder.project.nets:
        buildnet.write_net_functions_forward(builder, net)
    for net in builder.project.nets:
        write_net_class(builder, net)
        write_net_class_extension(builder, net)
        builder.write_class_end()
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_net_class(builder, net):
    buildnet.write_net_class(builder,
                             net,
                             namespace="cass",
                             write_constructor=False,
                             write_class_end=False)
    return

def write_pack_method(builder, net):
    builder.write_method_start("void pack(ca::Packer &packer)")
    for place in net.places:
         builder.line("ca::pack(packer, place_{0.id});", place)
    builder.write_method_end()

def write_state_analyzes_method(builder, net):
    builder.write_method_start("size_t get_token_count_in_place(int place_id)")
    for p in net.places:
        builder.line("if (place_id == {0.id}) return this->place_{0.id}.size();", p)
    builder.line("return 0;")
    builder.write_method_end()

def write_net_class_extension(builder, net):
    write_pack_method(builder, net)
    write_state_analyzes_method(builder, net)

def write_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder, "cass::init", start_process=False)
    builder.line("VerifConfiguration verif_configuration;")
    builder.line("cass::Core core(verif_configuration);")
    builder.line("core.generate();")
    builder.line("core.postprocess();")
    builder.line("return 0;")
    builder.block_end()

def write_statespace_program(builder):
    builder.thread_class = "cass::Thread"
    builder.pack_bindings = True

    build.write_header(builder)
    builder.line("#include <caverif.h>")
    write_core(builder)
    write_verif_configuration(builder)
    write_main(builder)
    buildnet.write_user_functions(builder)

def write_spawn(builder, net):
    builder.line("ca::NetBase * spawn_{0.id}(ca::ThreadBase *$thread, ca::NetDef *$def) {{", net)
    builder.indent_push()
    builder.line("{0} *$net = new {0}();", buildnet.get_net_class_name(net))
    buildnet.write_init_net(builder, net)
    builder.line("return $net;")
    builder.block_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        buildnet.write_transition_functions(builder, tr)
