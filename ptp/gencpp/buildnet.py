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
from base.neltypes import t_array
from writer import CppWriter
import build

def write_register_net(builder, net):
    builder.line("CaNetDef *def_{0.id} = new CaNetDef({1}, {0.id}, spawn_{0.id}, {2});",
                 net,
                 net.get_index(),
                 builder.emitter.const_boolean(net.is_local()))

    for i, tr in enumerate(net.transitions):
        builder.line("def_{0.id}->register_transition(&transition_{1.id});", net, tr)

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

def write_tokens_struct(builder, tr):
    """
        Writes struct that serves as storage for tokens between "find_binding" and "fire"
    """
    class_name = "Tokens_{0.id}".format(tr)
    builder.write_class_head(class_name)

    for edge in tr.get_normal_edges_in():
        place_t = builder.emit_type(edge.get_place_type())
        builder.line("CaToken<{0} > *token_{1.uid};", place_t, edge)

    for edge in tr.get_packing_edges_in():
        place_t = builder.emit_type(edge.get_place_type())
        builder.line("std::vector<{0} > packed_values_{1.uid};", place_t, edge)
    builder.write_class_end()

def write_transition_forward(builder, tr):
    if tr.code is not None:
        write_vars_struct(builder, tr)

    write_tokens_struct(builder, tr)

    class_name = "Transition_{0.id}".format(tr)
    builder.write_class_head(class_name, "CaTransitionDef")
    builder.line("int get_id() {{ return {0.id}; }}", tr)
    builder.line("int full_fire(CaThreadBase *thread, CaNetBase *net);")
    builder.line("void* fire_phase1(CaThreadBase *thread, CaNetBase *net);")
    builder.line("void fire_phase2(CaThreadBase *thread, CaNetBase *net, void *data);")
    builder.line("void cleanup_binding(void *data);")
    builder.line("bool is_enable(CaThreadBase *thread, CaNetBase *net);")
    if builder.generate_operator_eq:
        builder.line("bool binding_equality(void *data1, void *data2);")
    if builder.generate_hash:
        builder.line("size_t binding_hash(void *data);")
    builder.write_class_end()
    builder.line("static Transition_{0.id} transition_{0.id};", tr)
    builder.emptyline();

def write_transition_functions(builder,
                               tr,
                               locking=True):
    if tr.code is not None:
        write_transition_user_function(builder, tr)
    write_full_fire(builder, tr, locking=locking)
    write_fire_phase1(builder, tr)
    write_fire_phase2(builder, tr)
    write_cleanup_binding(builder, tr)
    write_enable_check(builder, tr)
    if builder.generate_operator_eq:
        write_binding_equality(builder, tr)
    if builder.generate_hash:
        write_binding_hash(builder, tr)

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

def write_send_token(builder,
                     em,
                     edge,
                     trace_send=False,
                     locking=True,
                     interface_edge=False,
                     readonly_tokens=False):
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

    def write_add(method):
        place = edge.get_place()
        if edge.token_source is not None and not readonly_tokens:
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

    builder.line("// Output edge id={0.id} uid={0.uid} expr={0.expr} target={0.target}", edge)

    method = "add" if edge.is_normal() else "add_all"

    if edge.guard is not None:
        builder.if_begin(edge.guard.emit(em))
    if edge.is_local():
        write_lock()
        write_add(method)
    else: # Remote send
        if edge.is_unicast():
            sendtype = ""
            builder.line("int target_{0.id} = {1};", edge, edge.target.emit(em))
            builder.if_begin("target_{0.id} == thread->get_process_id()".format(edge))
            write_lock()
            write_add(method)
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
        
        if trace_send:
            builder.if_begin("tracelog")
            builder.line("tracelog->event_send_msg(thread->get_new_msg_id());")
            builder.block_end()

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

def write_remove_tokens(builder, tr):
    for edge in tr.get_normal_edges_in():
        if edge.get_place().tracing:
            builder.if_begin("tracelog")
            builder.line("n->place_{1.id}.remove(token_{0.uid}, tracelog, {1.id});",
                edge, edge.get_place())
            builder.line("}} else {{")
            builder.line("n->place_{1.id}.remove(token_{0.uid});", edge, edge.get_place())
            builder.block_end()
        else:
            builder.line("n->place_{1.id}.remove(token_{0.uid});", edge, edge.get_place())

def write_fire_body(builder,
                    tr,
                    locking=True,
                    remove_tokens=True,
                    readonly_tokens=False,
                    packed_tokens_from_place=True):

    matches, _, _ = get_edges_mathing(builder.project, tr)
    if tr.need_trace():
        builder.line("CaTraceLog *tracelog = thread->get_tracelog();")
        builder.if_begin("tracelog")
        builder.line("tracelog->event_transition_fired({0.id});", tr)
        builder.block_end()

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
            builder.line("{1} out_value_{0};", token_out_counter, t)
            vars_code[name] = "out_value_{0}".format(token_out_counter);
            token_out_counter += 1
        else:
            edge = tr.var_edge[name]
            token = ExprExtern("token_{0.uid}".format(edge))
            vars_code[name] = edge.expr.get_variable_access(token, name)[0].emit(em)
            # Here we have taken first variable access, but more inteligent approacn
            # should be used

    em.variable_emitter = lambda name: vars_code[name]

    if remove_tokens:
        write_remove_tokens(builder, tr)

    builder.line("n->activate_transition_by_pos_id({0});", tr.get_pos_id())

    if packed_tokens_from_place:
        for edge in tr.get_packing_edges_in():
            builder.line("{1} = n->place_{0.id}.to_vector_and_clear();",
                   edge.get_place(), em.variable_emitter(edge.varname))
    else:
        for edge in tr.get_packing_edges_in():
            builder.line("{1} = tokens->packed_values_{0.uid};",
                   edge, em.variable_emitter(edge.varname))

    if tr.code is not None:
        if locking:
            builder.line("n->unlock();")
        if len(names) == 0:
            builder.line("Vars_{0.id} vars;", tr)
        else:
            builder.line("Vars_{0.id} vars({1});", tr, ",".join([ vars_code[n] for n in names ]))
        builder.line("transition_user_fn_{0.id}(ctx, vars);", tr)

        if locking:
            builder.line("bool lock = false;")
        if tr.need_trace():
            builder.if_begin("tracelog")
            builder.line("tracelog->event_transition_finished();")
            builder.block_end()
    elif locking:
        builder.line("bool lock = true;")

    for edge in tr.get_packing_edges_out() + tr.get_normal_edges_out():
        write_send_token(builder,
                         em,
                         edge,
                         trace_send=tr.need_trace(),
                         locking=locking,
                         readonly_tokens=readonly_tokens)
    if locking:
        builder.line("if (lock) n->unlock();")

    for edge, _ in matches:
        if not edge.token_reused and not readonly_tokens:
            builder.line("delete token_{0.uid};", edge)

def write_full_fire(builder, tr, locking=True):
    builder.line("int Transition_{0.id}::full_fire(CaThreadBase *t, CaNetBase *net)",
                 tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    builder.line("CaContext ctx(thread, net);")

    w = build.Builder(builder.project)
    write_fire_body(w, tr, locking=locking)
    w.line("return CA_TRANSITION_FIRED;")

    write_enable_pattern_match(builder, tr, w)
    builder.line("return CA_NOT_ENABLED;")
    builder.block_end()

def write_enable_check(builder, tr):
    builder.line("bool Transition_{0.id}::is_enable(CaThreadBase *t, CaNetBase *net)",
                 tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    w = CppWriter()
    w.line("return true;")
    write_enable_pattern_match(builder, tr, w)
    builder.line("return false;")
    builder.block_end()

def write_fire_phase1(builder, tr):
    builder.line("void *Transition_{0.id}::fire_phase1(CaThreadBase *t, CaNetBase *net)", tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    w = build.Builder(builder.project)
    if tr.need_trace():
        builder.line("CaTraceLog *tracelog = thread->get_tracelog();")
    write_remove_tokens(w, tr)
    w.line("Tokens_{0.id} *tokens = new Tokens_{0.id}();", tr)
    for edge in tr.get_normal_edges_in():
        w.line("tokens->token_{0.uid} = token_{0.uid};", edge);

    for edge in tr.get_packing_edges_in():
        w.line("tokens->packed_values_{0.uid} = n->place_{1.id}.to_vector_and_clear();",
               edge, edge.get_place())
    w.line("return tokens;")

    write_enable_pattern_match(builder, tr, w)
    builder.line("return NULL;")
    builder.block_end()

def write_fire_phase2(builder, tr):
    builder.line("void Transition_{0.id}::fire_phase2"
                    "(CaThreadBase *t, CaNetBase *net, void *data)",
                 tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    builder.line("CaContext ctx(thread, net);")
    builder.line("{0} *n = ({0}*) net;", get_net_class_name(tr.net))
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)
    for edge in tr.get_normal_edges_in():
        place_t = builder.emit_type(edge.get_place_type())
        builder.line("CaToken<{0} > *token_{1.uid} = tokens->token_{1.uid};", place_t, edge);

    write_fire_body(builder,
                    tr,
                    locking=False,
                    remove_tokens=False,
                    readonly_tokens=True,
                    packed_tokens_from_place=False)
    builder.block_end()

def write_cleanup_binding(builder, tr):
    builder.line("void Transition_{0.id}::cleanup_binding(void *data)", tr)
    builder.block_begin()
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)
    for edge in tr.get_normal_edges_in():
        place_t = builder.emit_type(edge.get_place_type())
        builder.line("delete tokens->token_{1.uid};", place_t, edge);
    builder.line("delete tokens;");
    builder.block_end()

def write_binding_equality(builder, tr):
    builder.line("bool Transition_{0.id}::binding_equality(void *data1, void *data2)", tr)
    builder.block_begin()
    builder.line("Tokens_{0.id} *tokens1 = (Tokens_{0.id}*) data1;", tr)
    builder.line("Tokens_{0.id} *tokens2 = (Tokens_{0.id}*) data2;", tr)
    conditions = [ "(tokens1->token_{0.uid}->value == tokens2->token_{0.uid}->value)"
                        .format(edge)
                   for edge in tr.get_normal_edges_in() ]
    conditions += [ "(tokens1->packed_values_{0.uid} == tokens2->packed_values_{0.uid})"
                        .format(edge)
                   for edge in tr.get_packing_edges_in() ]
    if conditions:
        builder.line("return {0};", "&&".join(conditions))
    else:
        builder.line("return true;")
    builder.block_end()

def write_binding_hash(builder, tr):
    builder.line("size_t Transition_{0.id}::binding_hash(void *data)", tr)
    builder.block_begin()
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)
    types_codes = [ (edge.get_place_type(), "tokens->token_{0.uid}->value".format(edge))
                    for edge in tr.get_normal_edges_in() ]
    types_codes += [ (t_array(edge.get_place_type()), "tokens->packed_values_{0.uid}".format(edge))
                     for edge in tr.get_packing_edges_in() ]
    builder.line("return {0};", build.get_hash_codes(builder.project, types_codes))
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

    builder.line("{0} *n = ({0}*) net;", get_net_class_name(tr.net))

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
    build.write_basic_definitions(builder)
    for net in builder.project.nets:
        write_net_functions_forward(builder, net)
    for net in builder.project.nets:
        write_net_class(builder, net)
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_place_add(builder, method, place, place_code, value_code):
    if place.tracing:
        builder.if_begin("thread->get_tracelog()")
        builder.line("void (*fncs[{0}])(CaTraceLog *, const {1} &);",
                     len(place.tracing),
                     builder.emit_type(place.type))
        for i, trace in enumerate(place.tracing):
            builder.line("fncs[{0}] = trace_{1};", i, trace.replace("fn: ", ""))
        builder.line("{0}{1}({2}, thread->get_tracelog(), {3.id}, {4}, fncs);",
                    place_code, method, value_code, place, len(place.tracing))
        builder.line("}} else {{")
        builder.line("{0}{1}({2});", place_code, method, value_code)
        builder.block_end()
    else:
        builder.line("{0}{1}({2});", place_code, method, value_code)

def write_init_net(builder, net):
    builder.line("CaContext ctx(thread, net);")
    builder.line("int pid = thread->get_process_id();")
    for area in net.areas:
        builder.line("std::vector<int> area_{0.id} = {1};",
                     area,
                     area.expr.emit(builder.emitter))
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

def write_spawn(builder, net):
    builder.line("CaNetBase * spawn_{0.id}(CaThreadBase *thread, CaNetDef *def) {{", net)
    builder.indent_push()
    builder.line("{0} *net = new {0}(def, ({1}*) thread);", get_net_class_name(net),
                                                            builder.thread_class)
    write_init_net(builder, net)
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
    builder.write_method_end()

def write_receive_method(builder, net):
    builder.write_method_start(
        "void receive(CaThreadBase *thread, int place_pos, CaUnpacker &unpacker)")
    builder.line("switch(place_pos) {{")
    for place in net.places:
        edges = place.get_edges_in(with_interface=True)
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

def write_net_functions_forward(builder, net):
    for place in net.places:
        if place.code is not None:
            write_place_user_function(builder, place)

    for tr in net.transitions:
        write_transition_forward(builder, tr)

def get_net_class_name(net):
    return "Net_" + str(net.id)

def write_net_class(builder, net, base_class_name="CaNet", write_class_end=True):
    builder.write_class_head(get_net_class_name(net), base_class_name)

    decls = [("def", "CaNetDef *"),
             ("thread", "CaThread *")]

    builder.write_constructor(get_net_class_name(net),
                              builder.emit_declarations(decls),
                              ["{0}(def, thread)".format(base_class_name)])
    builder.write_method_end()

    for place in net.places:
        builder.write_var_decl("place_" + str(place.id),
                               "CaPlace<{0} >".format(builder.emit_type(place.type)))

    write_reports_method(builder, net)
    write_receive_method(builder, net)

    if write_class_end:
        builder.write_class_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        write_transition_functions(builder, tr)

def write_main_setup(builder, init_function="ca_init"):
    builder.line("ca_project_description({0});",
        builder.emitter.const_string(builder.project.description))
    builder.line("std::vector<CaParameter*> parameters;")
    for p in builder.project.get_parameters():
        builder.line("parameters.push_back(&param::{0});", p.get_name())

    builder.emptyline()
    builder.line("{0}(argc, argv, parameters);", init_function)
    builder.emptyline()

    for net in builder.project.nets:
        write_register_net(builder, net)

    defs = [ "def_{0.id}".format(net) for net in builder.project.nets ]
    builder.line("CaNetDef *defs[] = {{{0}}};", ",".join(defs))
    builder.line("ca_setup({0}, defs);", len(defs));
