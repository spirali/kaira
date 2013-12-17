#
#    Copyright (C) 2012-2013 Stanislav Bohm
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

def write_dependent(builder):
    def write_transition_dependency(builder, t1, t2):
        # Transitions without known target
        if not (t1.has_fixed_target() and t2.has_fixed_target()) and not (t1.is_local() or t2.is_local()):
            builder.line("// Transitions without known target")
            builder.line("if (a2.data.fire.transition_def->get_id() == {0.id}) return true;", t2)
            return

        # Transitions taking tokens from the same input place
        if t1.get_input_places().intersection(t2.get_input_places()):
            builder.line("// Transitions taking tokens from the same input place")
            builder.line("if (a2.data.fire.transition_def->get_id() == {0.id}) return true;", t2)
            return

        # A transition putting tokens to a input place of a bulk transition
        edges1 = [ e2 for e1, e2 in utils.objects_with_same_attribute(t1.edges_in, t2.edges_out, lambda e: e.place) if e1.is_bulk()]
        edges2 = [ e2 for e1, e2 in utils.objects_with_same_attribute(t2.edges_in, t1.edges_out, lambda e: e.place) if e1.is_bulk()]
        for e in edges1 + edges2:
            if e.has_fixed_target() and all(i.target is not None for i in e.inscriptions):
                condition = " || ".join("a1.process == " + str(i.target) for i in e.inscriptions)
                builder.if_begin("a2.data.fire.transition_def->get_id() == {0.id}", t2)
                builder.line("// A transition putting tokens to a input place of a bulk transition")
                builder.line("if ({0}) return true;", condition)
                builder.block_end()
            else:
                builder.line("// A transition putting tokens to a input place of a bulk transition")
                builder.line("if (a2.data.fire.transition_def->get_id() == {0.id}) return true;", t2)
                return

        # Transitions putting tokens to the same local place
        for e1, e2 in utils.objects_with_same_attribute(t1.edges_out, t2.edges_out, lambda e: e.place):
            if e1.has_fixed_target() and e2.has_fixed_target():
                builder.if_begin("a2.data.fire.transition_def->get_id() == {0.id}", t2)
                if all(i.target is not None for i in e1.inscriptions):
                    condition1 = " || ".join("a1.process == " + str(i.target) for i in e1.inscriptions)
                else:
                    condition1 = "true"
                if all(i.target is not None for i in e2.inscriptions):
                    condition2 = " || ".join("a1.process == " + str(i.target) for i in e2.inscriptions)
                else:
                    condition2 = "true"
                builder.line("// Transitions putting tokens to the same local place")
                builder.line("if (({0}) && ({1})) return true;", condition1, condition2)
                builder.block_end()

        # Transitions putting tokens to the same process
        edges1 = [ e for e in t1.edges_out if e.has_fixed_target() and not e.is_local() ]
        edges2 = [ e for e in t2.edges_out if e.has_fixed_target() and not e.is_local() ]
        for e1 in edges1:
            for e2 in edges2:
                targets1 = [i.target for i in e1.inscriptions if i.target is not None]
                targets2 = [i.target for i in e2.inscriptions if i.target is not None]
                pairs = [ (tar1, tar2) for tar1 in targets1 for tar2 in targets2]
                builder.if_begin("a2.data.fire.transition_def->get_id() == {0.id}", t2)
                builder.line("// Transitions putting tokens to the same process")
                builder.line("if ({0}) return true;", " || ".join(str(c1) + " == " + str(c2) for c1 ,c2 in pairs))
                builder.block_end()

    def compare_fire_receive(builder, fire, receive):
        builder.switch_begin("a{0}.data.fire.transition_def->get_id()".format(fire))
        for net in builder.project.nets:
            for transition in net.transitions:
                builder.line("case {0.id}:", transition)
                builder.block_begin()
                # Receive token to the transition's output place
                for t_edge in transition.edges_out:
                    for r_edge in t_edge.place.get_edges_in():
                        builder.line("// t {0.transition.id} --> p {0.place.id}", r_edge)
                        builder.if_begin("a{1}.data.receive.edge_id == {0.id}", r_edge, receive)
                        targets = [ i.target for i in t_edge.inscriptions ]
                        if t_edge.has_fixed_target() and None not in targets:
                            builder.if_begin(" && ".join("a1.process != " + str(t) for t in targets))
                            builder.line("return false;")
                            builder.block_end()
                        builder.line("return true;")
                        builder.block_end()

                # Receive token to a place with transition's input place with bulk edge
                for t_edge in transition.get_bulk_edges_in():
                    for r_edge in t_edge.place.get_edges_in():
                        builder.line("// t {0.transition.id} --> p {0.place.id}", r_edge)
                        builder.line("if (a{1}.data.receive.edge_id == {0.id}) return true;", r_edge, receive)
                builder.line("return false;")
                builder.block_end()
        builder.block_end()
        builder.block_end()

    builder.line("bool is_dependent(const cass::Action &a1, const cass::Action &a2, cass::State &s)")
    builder.block_begin()
    builder.if_begin("a1.process != a2.process")
    builder.line("return false;")
    builder.block_end()
    builder.line("cass::VerifThread thread(a1.process, 0);")
    builder.line("ca::Context ctx(&thread, NULL);");

    builder.if_begin("a1.type == cass::ActionFire && a2.type == cass::ActionFire")
    builder.switch_begin("a1.data.fire.transition_def->get_id()")
    for net in builder.project.nets:
        for t1 in net.transitions:
            builder.line("case {0.id}:", t1)
            builder.block_begin()
            for t2 in net.transitions:
                if t1 is not t2:
                    write_transition_dependency(builder, t1, t2)
            builder.line("return false;")
            builder.block_end()

    builder.block_end()
    builder.block_end()

    builder.if_begin("a1.type == cass::ActionFire && a2.type == cass::ActionReceive")
    builder.line("cass::VerifThread thread(a1.process, 0);")
    builder.line("ca::Context ctx(&thread, NULL);");
    compare_fire_receive(builder, 1, 2)

    builder.if_begin("a1.type == cass::ActionReceive && a2.type == cass::ActionFire")
    compare_fire_receive(builder, 2, 1)

    builder.if_begin("a1.type == cass::ActionReceive && a2.type == cass::ActionReceive")
    builder.switch_begin("a1.data.receive.edge_id")
    for net in builder.project.nets:
        for edge in net.get_edges_out():
            if not edge.is_local():
                builder.line("case {0.id}: // t {0.transition.id} --> p {0.place.id}", edge)
                builder.block_begin()
                for e in edge.place.get_edges_in():
                    if not e.is_local():
                        builder.line("if (a2.data.receive.edge_id == {0.id}) return true;", e)
                builder.line("return false;")
                builder.block_end()
    builder.block_end()
    builder.block_end()
    builder.block_end()

def write_visible_transitions(builder):
    builder.line("cass::Action action;")
    builder.line("action.type = cass::ActionFire;")
    for net in builder.project.nets:
        for tr in net.transitions:
            if tr.calls_quit:
                builder.line("action.data.fire.transition_def = &transition_{0.id};", tr)
                builder.line("for (int i = 0; i < ca::process_count; i++)")
                builder.block_begin()
                builder.line("action.process = i;")
                builder.line("visible.insert(action);")
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
    write_visible_transitions(builder)
    if ignored:
        builder.line("int transitions[] = {{{0}}};", ", ".join(str(i.id) for i in ignored))
        builder.line("for (int i = 0; i < {0}; i++)", len(ignored))
        builder.block_begin()
        builder.line("ignored_transitions.insert(transitions[i]);")
        builder.block_end()
    builder.block_end();

def write_compute_successors(builder):
    def push_successor(builder, edge, net):
        builder.line("receiving = s.get_token_count_in_edge(action.process, a.process, {0.id});", edge)
        builder.line("place_tokens = ((Net_{0.id}*)s.get_net(action.process))->place_{1.id}.size();", net, edge.place)
        prefix = edge.place.get_token_prefix_size()
        if prefix is not None:
            builder.if_begin("receiving + place_tokens < {0}", prefix)
        builder.line("successors.push_back(action);")
        if prefix is not None:
            builder.block_end()

    builder.line("void compute_successors(const cass::Action &a, std::list<cass::Action> &successors, cass::State &s)")
    builder.block_begin()
    builder.line("cass::Action action;")
    builder.line("cass::VerifThread thread(a.process, 0);")
    builder.line("ca::Context ctx(&thread, NULL);");
    builder.line("int receiving, place_tokens;")
    builder.switch_begin("a.type")
    builder.line("case cass::ActionFire:")
    builder.block_begin()
    builder.switch_begin("a.data.fire.transition_def->get_id()")
    for net in builder.project.nets:
        for tr in net.transitions:
            builder.line("case {0.id}:", tr)
            builder.block_begin()
            for edge in tr.edges_out:
                builder.line("// t {0.transition.id} --> p {0.place.id}", edge)
                if edge.is_local():
                    for e in edge.place.get_edges_out():
                        if tr.id is not e.transition.id:
                            builder.line("action.process = a.process;")
                            builder.line("action.type = cass::ActionFire;")
                            builder.line("action.data.fire.transition_def = &transition_{0.id};", e.transition)
                            builder.if_not_begin("s.is_transition_enabled(action.process, action.data.fire.transition_def)")
                            builder.line("successors.push_back(action);")
                            builder.block_end()
                else:
                    builder.line("action.type = cass::ActionReceive;")
                    builder.line("action.data.receive.source = a.process;")
                    builder.line("action.data.receive.edge_id = {0.id};", edge)
                    if edge.has_fixed_target():
                        for insc in edge.inscriptions:
                            if insc.target is not None:
                                builder.line("action.process = {0};", insc.target)
                                push_successor(builder, edge, net)
                    else:
                        builder.line("for (int p = 0; p < ca::process_count; p++)")
                        builder.block_begin()
                        builder.line("action.process = p;")
                        push_successor(builder, edge, net)
                        builder.block_end()
            builder.line("return;")
            builder.block_end()
    builder.block_end()
    builder.line("return;")
    builder.block_end()

    builder.line("case cass::ActionReceive:")
    builder.block_begin()
    builder.line("int edge_id = s.get_receiving_edge(a.process, a.data.receive.source, 1);")
    builder.if_begin("edge_id != -1")
    builder.line("action.type = cass::ActionReceive;")
    builder.line("action.process = a.process;")
    builder.line("action.data.receive.source = a.data.receive.source;")
    builder.line("action.data.receive.edge_id = edge_id;")
    builder.line("successors.push_back(action);")
    builder.block_end()
    builder.line("action.type = cass::ActionFire;")
    builder.line("action.process = a.process;")
    builder.switch_begin("a.data.receive.edge_id")
    for net in builder.project.nets:
        for edge in net.get_edges_out():
            if edge.is_local():
                continue
            builder.line("case {0.id}: // t {0.transition.id} --> p {0.place.id}", edge)
            builder.block_begin()
            if edge.place.get_token_prefix_size():
                builder.if_begin("((Net_{0.id}*)s.get_net(a.process))->place_{1.id}.size() >= {2}", net, edge.place, edge.place.get_token_prefix_size())
                builder.line("return;")
                builder.block_end()
            for tr in edge.place.get_transitions_out():
                builder.line("action.data.fire.transition_def = &transition_{0.id};", tr)
                builder.line("successors.push_back(action);")
            builder.line("return;")
            builder.block_end()
    builder.block_end()
    builder.line("return;")
    builder.block_end()
    builder.block_end()
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
    write_compare_function(builder, compared)
    builder.line("private:")
    write_compute_successors(builder)

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

def write_net_class_extension(builder, net):
    write_pack_method(builder, net)

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
        buildnet.write_transition_functions(builder,
                                            tr,
                                            locking=False)
