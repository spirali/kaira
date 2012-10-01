#
#    Copyright (C) 2012 Stanislav Bohm
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


import base.utils as utils
from base.gentools import get_edges_mathing
import emitter
from base.expressions import ExprExtern
from writer import CppWriter
import build

def write_register_net(builder, net):
    builder.line("CaNetDef *def_{0.id} = new CaNetDef({2}, {0.id}, {1}, spawn_{0.id}, {3});",
                 net,
                 len(net.transitions),
                 net.get_index(),
                 builder.emitter.const_boolean(net.is_local()))

    for i, tr in enumerate(net.transitions):
        builder.line("def_{0.id}->register_transition({2}, {1.id},(CaEnableFn*) enable_{1.id},"
                  " enable_check_{1.id});", net, tr, i)

def write_vars_struct(builder, tr):
    """
        Write class that servers as interface for transition's inner functions.
    """

    class_name = "Vars_{0.id}".format(tr)
    builder.write_class_head(class_name)

    decls = tr.get_context().items()
    decls.sort(key=lambda x: x[0]) # Sort by variables names

    builder.write_constructor(class_name, builder.emit_declarations(decls, True),
                           ["{0}({0})".format(name) for name, _ in decls ])
    builder.write_method_end()

    for name, t in decls:
        builder.write_var_decl(name, builder.emit_type(t), True)
    builder.write_class_end()

def write_transition_forward(builder, tr):
    if tr.code is not None:
        write_vars_struct(builder, tr)
    builder.line("bool enable_check_{0.id}(CaThread *thread, CaNet *net);", tr)

def write_transition(builder, tr):
    if tr.code is not None:
        write_transition_user_function(builder, tr)
    write_enable(builder, tr)
    write_enable_check(builder, tr)

def write_transition_user_function(builder, tr):
    declaration = "void transition_user_fn_{0.id}(CaContext &ctx, Vars_{0.id} &var)".format(tr)
    builder.write_function(declaration, tr.code, ("*{0.id}/function".format(tr), 1))

def write_place_user_function(builder, place):
    t = builder.emit_type(place.type)
    declaration = "void place_user_fn_{0.id}(CaContext &ctx, std::vector<{1} > &tokens)" \
                        .format(place, t)
    builder.write_function(declaration, place.code, ("*{0.id}/init_function".format(place), 1))

def write_activation(builder, net, transitions):
    for tr in transitions:
        builder.line("{0}->activate_transition_by_pos_id({1});", net, tr.get_pos_id())

def write_send_token(builder, em, edge, locking = True, interface_edge = False):

    def write_lock():
        if not locking:
            return
        builder.if_begin("!lock")
        builder.line("n->lock();")
        builder.line("lock = true;")
        builder.block_end()

    def write_unlock():
        if not locking:
            return
        builder.if_begin("lock")
        builder.line("n->unlock();")
        builder.line("lock = false;")
        builder.block_end()

    method = "add" if edge.is_normal() else "add_all"

    if edge.guard is not None:
        builder.if_begin(edge.guard.emit(em))
    if edge.is_local():
        write_lock()
        place = edge.get_place()
        if edge.token_source is not None:
            write_place_add(builder,
                            method + "_token",
                            place,
                            "n->place_{0.id}.".format(place),
                            ExprExtern("token_{0.uid}_builder"
                                        .format(edge.token_source)).emit(em))
        else:
            write_place_add(builder,
                            method,
                            place,
                            "n->place_{0.id}.".format(place),
                            edge.expr.emit(em))
        if not interface_edge:
            write_activation(builder, "n", edge.get_place().get_transitions_out())
    else:
        if edge.is_unicast():
            sendtype = ""
            builder.line("int target_{0.id} = {1};", edge, edge.target.emit(em))
            builder.if_begin("target_{0.id} == thread->get_process_id()".format(edge))
            write_lock()
            builder.line("n->place_{0.id}.{2}({1});", edge.get_place(), edge.expr.emit(em), method)
            if not interface_edge:
                write_activation(builder, "n", edge.get_place().get_transitions_out())
            builder.indent_pop()
            builder.line("}} else {{")
            builder.indent_push()
        else:
            builder.line("std::vector<int> target_{0.id} = {1};", edge, edge.target.emit(em))
            sendtype = "_multicast"
            builder.block_begin()

        write_unlock()
        t = edge.get_place_type()
        traw = builder.emit_type(t)
        builder.line("{0} value = {1};",
                     builder.emit_type(edge.expr.nel_type), edge.expr.emit(em))
        if edge.is_normal(): # Pack normal edge
            builder.line("CaPacker packer(CA_PACKER_DEFAULT_SIZE, CA_RESERVED_PREFIX);")
            builder.line("{0};", build.get_pack_code(builder.project, t, "packer", "value"))
            builder.line("thread->send{0}(target_{1.id}, n, {2}, packer);",
                   sendtype, edge, edge.get_place().get_pos_id())
        else: # Pack packing edge
            # TODO: Pack in one step if type is directly packable
            builder.line("CaPacker packer(CA_PACKER_DEFAULT_SIZE, CA_RESERVED_PREFIX);")
            builder.line(
                "for (std::vector<{0} >::iterator i = value.begin(); i != value.end(); i++)",
                traw)
            builder.block_begin()
            builder.line("{0};", build.get_pack_code(builder.project, t, "packer", "(*i)"))
            builder.block_end()
            builder.line("thread->multisend{0}(target_{1.id}, n, {2}, value.size(), packer);",
                   sendtype,edge, edge.get_place().get_pos_id())
        builder.block_end()
    if edge.guard is not None:
        builder.block_end()

def write_enable(builder, tr):
    builder.line("int enable_{0.id}(CaThread *thread, CaNet *net)", tr)
    builder.block_begin()
    builder.line("CaContext ctx(thread, net);")

    w = build.Builder(builder.project)
    matches, _, _ = get_edges_mathing(builder.project, tr)
    if tr.tracing != "off" or tr.is_any_place_traced():
        w.line("CaTraceLog *tracelog = thread->get_tracelog();")
        w.if_begin("tracelog")
        w.line("tracelog->event_transition_fired({0.id});", tr)
        w.block_end()

    em = emitter.Emitter(builder.project)
    for edge, _ in matches:
        em.set_extern("token_{0.uid}".format(edge), "token_{0.uid}->value".format(edge))
        em.set_extern("token_{0.uid}_builder".format(edge), "token_{0.uid}".format(edge))

    context = tr.get_context()
    names = context.keys()
    names.sort()

    token_out_counter = 0
    vars_code = {}

    for name in names:
        if name not in tr.var_edge: # Variable is not obtained from output
            t = builder.emit_type(context[name])
            w.line("{1} out_value_{0};", token_out_counter, t)
            #w.line("CaToken<{1}> token_out_{0}(value);", token_out_counter, t)
            vars_code[name] = "out_value_{0}".format(token_out_counter);
            token_out_counter += 1
        else:
            edge = tr.var_edge[name]
            token = ExprExtern("token_{0.uid}".format(edge))
            vars_code[name] = edge.expr.get_variable_access(token, name)[0].emit(em)
            # Here we have taken first variable access, but more inteligent approacn
            # should be used

    em.variable_emitter = lambda name: vars_code[name]

    for edge, _ in matches:
        if edge.get_place().tracing != "off":
            w.if_begin("tracelog")
            w.line("n->place_{1.id}.remove(token_{0.uid}, tracelog, {1.id});",
                edge, edge.get_place())
            w.line("}} else {{")
            w.line("n->place_{1.id}.remove(token_{0.uid});", edge, edge.get_place())
            w.block_end()
        else:
            w.line("n->place_{1.id}.remove(token_{0.uid});", edge, edge.get_place())

    w.line("net->activate_transition_by_pos_id({0});", tr.get_pos_id())

    for edge in tr.get_packing_edges_in():
        w.line("{1} = n->place_{0.id}.to_vector_and_clear();",
               edge.get_place(), em.variable_emitter(edge.varname))

    retvalue = "CA_TRANSITION_FIRED"
    if tr.code is not None:
        w.line("net->unlock();")
        if len(names) == 0:
            w.line("Vars_{0.id} vars;", tr)
        else:
            w.line("Vars_{0.id} vars({1});", tr, ",".join([ vars_code[n] for n in names ]))
        w.line("transition_user_fn_{0.id}(ctx, vars);", tr)

        w.line("bool lock = false;")
        if tr.tracing != "off" or tr.is_any_place_traced():
            w.if_begin("tracelog")
            w.line("tracelog->event_transition_finished();")
            w.block_end()
    else:
        w.line("bool lock = true;")

    for edge in tr.get_packing_edges_out() + tr.get_normal_edges_out():
        write_send_token(w, em, edge)

    w.line("if (lock) n->unlock();")

    for edge, _ in matches:
        if not edge.token_reused:
            w.line("delete token_{0.uid};", edge)

    w.line("return {0};", retvalue)

    write_enable_pattern_match(builder, tr, w)
    builder.line("return CA_NOT_ENABLED;")
    builder.block_end()

def write_enable_check(builder, tr):
    builder.line("bool enable_check_{0.id}(CaThread *thread, CaNet *net) {{", tr)
    builder.indent_push()

    w = CppWriter()
    w.line("return true;")
    write_enable_pattern_match(builder, tr, w)
    builder.line("return false;")
    builder.block_end()

def write_enable_pattern_match(builder, tr, fire_code):
    def variable_emitter(name):
        edge = tr.var_edge[name]
        token = ExprExtern("token_{0.uid}".format(edge))
        return edge.expr.get_variable_access(token, name)[0].emit(em)

    matches, initcode, _ = get_edges_mathing(builder.project, tr)

    em = emitter.Emitter(builder.project)

    for edge, _ in matches:
        em.set_extern("token_{0.uid}".format(edge), "token_{0.uid}->value".format(edge))
        em.set_extern("token_{0.uid}_builder".format(edge), "token_{0.uid}".format(edge))

    em.variable_emitter = variable_emitter

    builder.line("Net_{0.id} *n = (Net_{0.id}*) net;", tr.net)

    need_tokens = utils.multiset([ edge.get_place() for edge, instrs in matches ])

    # Check if there are enough tokens
    for place, count in need_tokens.items():
        builder.line("if (n->place_{0.id}.size() < {1}) return false;", place, count)

    em.set_extern("fail", "return false;")
    for i in initcode:
        i.emit(em, builder)

    for i, (edge, instrs) in enumerate(matches):
        builder.line("// Edge id={0.id} uid={0.uid} expr={0.expr}", edge)
        place_t = builder.emit_type(edge.get_place_type())
        place_id = edge.get_place().id
        token = "token_{0.uid}".format(edge)
        builder.line("CaToken<{0} > *{1} = n->place_{2}.begin();", place_t, token, place_id)
        em.set_extern("fail", "{0} = {0}->next; continue;".format(token))
        builder.do_begin()

        checks = [ "{0} == token_{1.uid}".format(token, e)
                    for e, _ in matches[:i]
                    if edge.get_place() == e.get_place() ]
        if checks:
            builder.if_begin(" || ".join(checks))
            builder.line("{0} = {0}->next;", token)
            builder.line("continue;")
            builder.block_end()

        for instr in instrs:
            instr.emit(em, builder)

    for edge in tr.get_packing_edges_in():
        need = need_tokens.get(edge.get_place(), 0)
        builder.if_begin("n->place_{0.id}.size() < {1} + {2}".format(edge.get_place(),
                                                                     need,
                                                                     edge.limit.emit(em)))
        if matches:
            builder.line("token_{0.uid} = token_{0.uid}->next;", matches[-1][0])
            builder.line("continue;")
        else:
            builder.line("return false;")
        builder.block_end()

    builder.block_begin()
    builder.add_writer(fire_code)
    builder.block_end()

    # End of matching cycles
    for edge, _ in reversed(list(matches)):
        builder.line("token_{0.uid} = token_{0.uid}->next;", edge)
        builder.do_end("token_{0.uid} != n->place_{1.id}.begin()".format(edge, edge.get_place()))

def write_core(builder):
    build.write_parameters(builder)
    build.write_types(builder)
    build.write_user_functions(builder)
    for net in builder.project.nets:
        write_net_class(builder, net)
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_place_add(builder, method, place, place_code, value_code):
    if place.tracing != "off":
        builder.if_begin("thread->get_tracelog()")
        builder.line("{0}{1}({2}, thread->get_tracelog(), {3.id}, {4});",
                  place_code, method, value_code, place,
                  build.get_to_string_function_name(builder.project, place.type))
        builder.line("}} else {{")
        builder.line("{0}{1}({2});", place_code, method, value_code)
        builder.block_end()
    else:
        builder.line("{0}{1}({2});", place_code, method, value_code)

def write_spawn(builder, net):
    builder.line("CaNet * spawn_{0.id}(CaThread *thread, CaNetDef *def) {{", net)
    builder.indent_push()
    builder.line("Net_{0.id} *net = new Net_{0.id}(def, thread);", net)
    builder.line("CaContext ctx(thread, net);")
    builder.line("int pid = thread->get_process_id();")
    for area in net.areas:
        builder.line("std::vector<int> area_{0.id} = {1};", area, area.expr.emit(builder.emitter))
    for place in net.places:
        if not (place.init_expression or place.code):
            continue
        areas = place.get_areas()
        if areas == []:
            builder.if_begin("pid == 0")
        else:
            conditions = [ "std::find(area_{0.id}.begin(), area_{0.id}.end(), pid) !="
                           " area_{0.id}.end()"
                          .format(area) for area in areas ]
            builder.if_begin(" && ".join(conditions))

        if place.init_expression is not None:
            write_place_add(builder, "add_all", place, "net->place_{0.id}.".format(place),
                               place.init_expression.emit(builder.emitter))
        if place.code is not None:
            t = builder.emit_type(place.type)
            builder.line("std::vector<{0} > tokens;", t)
            builder.line("place_user_fn_{0.id}(ctx, tokens);", place)
            write_place_add(builder, "add_all", place,
                            "net->place_{0.id}.".format(place), "tokens")
        builder.block_end()
    builder.line("return net;")
    builder.block_end()

def write_reports_method(builder, net):
    builder.write_method_start("void write_reports_content(CaThread *thread, CaOutput &output)")
    for place in net.places:
        builder.line('output.child("place");')
        builder.line('output.set("id", {0.id});', place)
        builder.block_begin()
        builder.line('CaToken<{1} > *t = place_{0.id}.begin();',
                     place, builder.emit_type(place.type))
        builder.if_begin("t")

        builder.do_begin()
        builder.line('output.child("token");')
        builder.line('output.set("value", {0});',
                     build.get_code_as_string(builder.project, "t->value", place.type))
        builder.line('output.back();')
        builder.line("t = t->next;")
        builder.do_end("t != place_{0.id}.begin()".format(place))
        builder.block_end()
        builder.block_end()
        builder.line('output.back();')
    for tr in net.transitions:
        builder.line("if (enable_check_{0.id}(thread, this)) {{", tr)
        builder.indent_push()
        builder.line('output.child("enabled");')
        builder.line('output.set("id", {0.id});', tr)
        builder.line('output.back();')
        builder.block_end()
    builder.write_method_end()

def write_receive_method(builder, net):
    builder.write_method_start(
        "void receive(CaThread *thread, int place_pos, CaUnpacker &unpacker)")
    builder.line("switch(place_pos) {{")
    for place in net.places:
        edges = place.get_edges_in(with_interface = True)
        if any((edge.target is not None for edge in edges)):
            builder.line("case {0}:", place.get_pos_id())
            builder.indent_push()
            write_place_add(builder,
                            "add",
                            place,
                            "place_{0.id}.".format(place),
                            build.get_unpack_code(builder.project, place.type, "unpacker"))
            write_activation(builder, "this", place.get_transitions_out())
            builder.line("break;")
            builder.indent_pop()
    builder.line("}}")
    builder.write_method_end()

def write_net_class(builder, net):

    for place in net.places:
        if place.code is not None:
            write_place_user_function(builder, place)

    for tr in net.transitions:
        write_transition_forward(builder, tr)

    class_name = "Net_" + str(net.id)
    builder.write_class_head(class_name, "CaNet")

    decls = [("def", "CaNetDef *"),
             ("thread", "CaThread *")]
    builder.write_constructor(class_name, builder.emit_declarations(decls),
                           ["CaNet(def, thread)"])
    builder.write_method_end()

    for place in net.places:
        builder.write_var_decl("place_" + str(place.id),
                               "CaPlace<{0} >".format(builder.emit_type(place.type)))

    write_reports_method(builder, net)
    write_receive_method(builder, net)
    builder.write_class_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        write_transition(builder, tr)
