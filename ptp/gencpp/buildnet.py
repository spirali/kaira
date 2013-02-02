#
#    Copyright (C) 2012, 2013 Stanislav Bohm
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
from writer import CppWriter, emit_declarations
from writer import const_string, const_boolean
import build

def write_register_net(builder, net):
    builder.line("ca::NetDef *def_{0.id} = new ca::NetDef({1}, {0.id}, spawn_{0.id}, {2});",
                 net,
                 net.get_index(),
                 const_boolean(net.is_local()))

    for i, tr in enumerate(net.transitions):
        builder.line("def_{0.id}->register_transition(&transition_{1.id});", net, tr)

def write_vars_struct(builder, tr):
    """
        Write class that servers as interface for transition's inner functions.
    """

    class_name = "Vars_{0.id}".format(tr)
    builder.write_class_head(class_name)

    decls = tr.get_decls().get_list()
    builder.write_constructor(class_name, emit_declarations(decls, True),
                           ["{0}({0})".format(name) for name, _ in decls ])
    builder.write_method_end()

    for name, t in decls:
        builder.write_var_decl(name, t, True)
    builder.write_class_end()

def write_tokens_struct(builder, tr):
    """
        Writes struct that serves as storage for tokens between "find_binding" and "fire"
    """
    class_name = "Tokens_{0.id}".format(tr)
    builder.write_class_head(class_name)

    for inscription in tr.get_token_inscriptions_in():
        builder.line("ca::Token<{0} > *token_{1};",
            inscription.get_type(), inscription.uid)

    """
    for edge in tr.get_packing_edges_in():
        builder.line("std::vector<{0} > packed_values_{1.uid};",
            edge.get_place_type(), edge)
    """

    builder.write_class_end()

def write_transition_forward(builder, tr):
    if tr.code is not None:
        write_vars_struct(builder, tr)

    write_tokens_struct(builder, tr)

    class_name = "Transition_{0.id}".format(tr)
    builder.write_class_head(class_name, "ca::TransitionDef")
    builder.line("int get_id() {{ return {0.id}; }}", tr)
    builder.line("ca::FireResult full_fire(ca::ThreadBase *thread, ca::NetBase *net);")
    builder.line("void* fire_phase1(ca::ThreadBase *thread, ca::NetBase *net);")
    builder.line("void fire_phase2(ca::ThreadBase *thread, ca::NetBase *net, void *data);")
    builder.line("void cleanup_binding(void *data);")
    builder.line("bool is_enable(ca::ThreadBase *thread, ca::NetBase *net);")
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
    declaration = "void transition_user_fn_{0.id}(ca::Context &ctx, Vars_{0.id} &var)".format(tr)
    builder.write_function(declaration, tr.code, ("*{0.id}/function".format(tr), 1))

def write_place_user_function(builder, place):
    declaration = "void place_user_fn_{0.id}(ca::Context &ctx, ca::TokenList<{1} > &place)" \
                        .format(place, place.type)
    builder.write_function(declaration, place.code, ("*{0.id}/init_function".format(place), 1))

def write_activation(builder, net, transitions):
    for tr in transitions:
        builder.line("{0}->activate_transition_by_pos_id({1});", net, tr.get_pos_id())

def write_send_token(builder,
                     edge,
                     net_expr,
                     trace_send=False,
                     locking=True,
                     interface_edge=False,
                     readonly_tokens=False,
                     reuse_tokens=None):

    def write_lock():
        if not locking:
            return
        builder.if_begin("!lock")
        builder.line("{0}->lock();", net_expr)
        builder.line("lock = true;")
        builder.block_end()

    def write_unlock():
        if not locking:
            return
        builder.if_begin("lock")
        builder.line("{0}->unlock();", net_expr)
        builder.line("lock = false;")
        builder.block_end()

    def write_add(expr, bulk=False, token=False):
        write_place_add(builder,
                        edge.place,
                        "{0}->".format(net_expr),
                        expr,
                        bulk=bulk,
                        token=token)
        if not interface_edge:
            write_activation(builder, net_expr, edge.place.get_transitions_out())

    if reuse_tokens is None:
        reuse_tokens = {}

    if edge.is_local():
        write_lock()
        if edge.is_bulk_edge():
            write_add(edge.inscriptions[0].expr, True)
        else:
            for inscription in edge.get_token_inscriptions():
                if inscription.uid in reuse_tokens:
                    write_add(build.get_safe_id("token_{0}" \
                                            .format(reuse_tokens[inscription.uid])),
                              token=True)
                else:
                    write_add(inscription.expr)
    else: # Remote send
        if edge.is_unicast():
            sendtype = ""
            builder.if_begin("{0} == thread->get_process_id()".format(edge.target))
            write_lock()
            if edge.is_bulk_edge():
                write_add(edge.inscriptions[0].expr, bulk=True)
            else:
                for inscription in edge.get_token_inscriptions():
                    write_add(inscription.expr)
            builder.indent_pop()
            builder.line("}} else {{")
            builder.indent_push()
        else:
            sendtype = "_multicast"
            builder.block_begin()

        write_unlock()
        if trace_send:
            builder.if_begin("tracelog")
            builder.line("tracelog->event_send_msg(thread->get_new_msg_id());")
            builder.block_end()

        if edge.is_token_edge(): # Pack normal edge
            builder.line("ca::Packer packer(ca::PACKER_DEFAULT_SIZE, ca::RESERVED_PREFIX);")
            for inscription in edge.get_token_inscriptions():
                builder.line("ca::pack(packer, {0});", inscription.expr)
            builder.line("thread->multisend{0}({1}, {4}, {2}, {3}, packer);",
                         sendtype,
                         edge.target,
                         edge.place.get_pos_id(),
                         len(edge.inscriptions),
                         net_expr)
        else: # Pack packing edge
            # TODO: Pack in one step if type is directly packable
            expr = edge.inscriptions[0].expr
            builder.line("ca::Packer packer(ca::PACKER_DEFAULT_SIZE, ca::RESERVED_PREFIX);")
            builder.line("{0}.pack_tokens(packer);", expr);
            builder.line("thread->multisend{0}({1}, {3}, {2}, ({4}).size(), packer);",
                   sendtype, edge.target, edge.place.get_pos_id(), net_expr, expr)
        builder.block_end()

    #if edge.guard is not None:
    #    builder.block_end()

def write_remove_tokens(builder, net_expr, tr):
    for inscription in tr.get_token_inscriptions_in():
        token_var = build.get_safe_id("token_{0.uid}".format(inscription))
        place = inscription.edge.place
        if place.tracing:
            builder.if_begin("tracelog")
            builder.line("{0}->place_{1.id}.remove({1}, tracelog, {2.id});",
                net_expr, token_var, place)
            builder.line("}} else {{")
            builder.line("{0}->place_{2.id}.remove({1});",
                net_expr, token_var, place)
            builder.block_end()
        else:
            builder.line("{0}->place_{2.id}.remove({1});",
                net_expr, token_var, place)

    for edge in tr.edges_in:
        place = edge.place
        if edge.is_origin_reader():
            if edge.is_bulk_edge():
                builder.line("{0}->place_{1.id}_origin.clear();", net_expr, place)
            else:
                for inscription in edge.inscriptions:
                    builder.line("{0}->place_{1.id}_origin.pop_front();",
                                 net_expr, place)


def write_fire_body(builder,
                    tr,
                    locking=True,
                    remove_tokens=True,
                    readonly_tokens=False,
                    packed_tokens_from_place=True):

    net_expr = build.get_safe_id("n")

    if tr.need_trace():
        builder.line("ca::TraceLog *tracelog = thread->get_tracelog();")
        builder.if_begin("tracelog")
        builder.line("tracelog->event_transition_fired({0.id});", tr)
        builder.block_end()

    if remove_tokens:
        write_remove_tokens(builder, build.get_safe_id("n"), tr)

    builder.line("{0}->activate_transition_by_pos_id({1});",
        net_expr, tr.get_pos_id())

    if packed_tokens_from_place:
        for edge in tr.get_bulk_edges_in():
                builder.line("{2} {3}({0}->place_{1.id}, true);",
                       net_expr, edge.place, edge.get_type(), edge.inscriptions[0].expr)
    else:
        for edge in tr.get_bulk_edges_in():
            builder.line("{1} {2} = tokens->packed_values_{0.uid}",
                edge, edge.get_type(), edge.inscriptions[0].expr)

    decls = tr.get_decls()

    for uid, t in tr.fresh_tokens:
        token_var = build.get_safe_id("token_{0}".format(uid))
        builder.line("ca::Token <{0} > *{1} = new ca::Token<{0} >;",
                     t,
                     token_var)


    for name, uid in tr.variable_sources_out.items():
        if uid is not None:
            token_var = build.get_safe_id("token_{0}".format(uid))
            builder.line("{0} &{1} = {2}->value;", decls[name], name, token_var)
        else:
            builder.line("{0} {1}; // Fresh variable", decls[name], name)


    if tr.code is not None:
        if locking:
            builder.line("{0}->unlock();", net_expr)
        decls = tr.get_decls().get_list()
        if len(decls) == 0:
            builder.line("Vars_{0.id} vars;", tr)
        else:
            builder.line("Vars_{0.id} vars({1});",
                tr, ",".join([ name for name, t in decls ]))
        builder.line("transition_user_fn_{0.id}(ctx, vars);", tr)
        if locking:
            builder.line("bool lock = false;")
        if tr.need_trace():
            builder.if_begin("tracelog")
            builder.line("tracelog->event_transition_finished();")
            builder.block_end()
    elif locking:
        builder.line("bool lock = true;")


    for edge in tr.edges_out:
        write_send_token(builder,
                         edge,
                         net_expr,
                         trace_send=tr.need_trace(),
                         locking=locking,
                         readonly_tokens=readonly_tokens,
                         reuse_tokens=tr.reuse_tokens)
    if locking:
        builder.line("if (lock) {0}->unlock();", net_expr)

    if not readonly_tokens:
        for edge in tr.edges_in:
            for inscription in edge.get_token_inscriptions():
                if inscription.uid not in tr.reuse_tokens.values():
                    token_var = build.get_safe_id("token_{0}".format(inscription.uid))
                    builder.line("delete {0};", token_var)

    """
    matches, _, _ = get_edges_mathing(builder.project, tr)
    if tr.need_trace():
        builder.line("ca::TraceLog *tracelog = thread->get_tracelog();")
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
    """

def write_full_fire(builder, tr, locking=True):
    builder.line("ca::FireResult Transition_{0.id}::full_fire(ca::ThreadBase *t, ca::NetBase *net)",
                 tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    builder.line("ca::Context ctx(thread, net);")

    w = build.Builder(builder.project)
    write_fire_body(w, tr, locking=locking)
    w.line("return ca::TRANSITION_FIRED;")

    write_enable_pattern_match(builder, tr, w, "return ca::NOT_ENABLED;")
    builder.line("return ca::NOT_ENABLED;")
    builder.block_end()

def write_enable_check(builder, tr):
    builder.line("bool Transition_{0.id}::is_enable(ca::ThreadBase *t, ca::NetBase *net)",
                 tr)
    builder.block_begin()
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    builder.line("ca::Context ctx(thread, net);")
    w = CppWriter()
    w.line("return true;")
    write_enable_pattern_match(builder, tr, w, "return false;")
    builder.line("return false;")
    builder.block_end()

def write_fire_phase1(builder, tr):
    builder.line("void *Transition_{0.id}::fire_phase1(ca::ThreadBase *t, ca::NetBase *net)", tr)
    builder.block_begin()
    """
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    w = build.Builder(builder.project)
    if tr.need_trace():
        builder.line("ca::TraceLog *tracelog = thread->get_tracelog();")
    write_remove_tokens(w, tr)
    w.line("Tokens_{0.id} *tokens = new Tokens_{0.id}();", tr)
    for edge in tr.get_normal_edges_in():
        w.line("tokens->token_{0.uid} = token_{0.uid};", edge);

    for edge in tr.get_packing_edges_in():
        w.line("tokens->packed_values_{0.uid} = n->place_{1.id}.to_vector_and_clear();",
               edge, edge.get_place())
    w.line("return tokens;")

    write_enable_pattern_match(builder, tr, w, "return NULL;")
    """
    builder.line("return NULL;")
    builder.block_end()

def write_fire_phase2(builder, tr):
    builder.line("void Transition_{0.id}::fire_phase2"
                    "(ca::ThreadBase *t, ca::NetBase *net, void *data)",
                 tr)
    builder.block_begin()
    """
    builder.line("{0} *thread = ({0}*) t;", builder.thread_class)
    builder.line("ca::Context ctx(thread, net);")
    n = build.get_safe_id("n")
    builder.line("{1} *{0} = ({1}*) net;", n, get_net_class_name(tr.net))
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)
    for edge in tr.get_normal_edges_in():
        builder.line("ca::Token<{0} > *token_{1.uid} = tokens->token_{1.uid};",
            edge.get_place_type(), edge);

    write_fire_body(builder,
                    tr,
                    locking=False,
                    remove_tokens=False,
                    readonly_tokens=True,
                    packed_tokens_from_place=False)
    """
    builder.block_end()

def write_cleanup_binding(builder, tr):
    builder.line("void Transition_{0.id}::cleanup_binding(void *data)", tr)
    builder.block_begin()
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)
    for inscription in tr.get_token_inscriptions_in():
        builder.line("delete tokens->token_{1.uid};",
            inscription.get_type(), inscription);
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

def write_enable_pattern_match(builder, tr, fire_code, fail_command):
    n, var = map(build.get_safe_id, [ "n", "var" ])
    builder.line("{0} *{1} = ({0}*) net;", get_net_class_name(tr.net), n)

    # Check if there are enough tokens
    for edge in tr.edges_in:
        if edge.get_tokens_number() is not None:
            builder.line("if ({0}->place_{1.id}.size() < {2}) {3}",
                n, edge.place, edge.get_tokens_number(), fail_command)

    for edge in tr.edges_in:
        prev_var = None
        for inscription in edge.get_token_inscriptions():
            token_var = build.get_safe_id("token_{0}".format(inscription.uid))
            builder.line("// Edge id={0.id} uid={1.uid} expr={1.expr}",
                edge, inscription)
            line = "ca::Token < {0} > *{1} = ".format(edge.get_place_type(), token_var)
            if prev_var is None:
                builder.line("{0} {1}->place_{2.id}.begin();",
                            line, n, edge.place)
            else:
                builder.line("{0} {1}->next;", line, prev_var)
            prev_var = token_var

    sources_uid = [ uid for uid in tr.variable_sources.values() if uid is not None ]
    decls_dict = tr.get_decls()

    for name, uid in tr.variable_sources.items():
        if uid is not None:
            token_var = build.get_safe_id("token_{0}".format(uid))
            builder.line("{0} &{1} = {2}->value;", decls_dict[name], name, token_var)

    for edge in tr.edges_in:
        if edge.is_origin_reader():
            if edge.is_bulk_edge():
              builder.line("std::vector<int> {0}_origins({1}->place_{2.id}_origin.begin(),"
                           " {1}->place_{2.id}_origin.end());",
                           edge.inscriptions[0].expr, n, edge.place)

            else:
                for i, inscription in enumerate(edge.inscriptions):
                    builder.line("int {0}_origin = *({1}->place_{2.id}_origin.begin() + {3});",
                                 inscription.expr, n, edge.place, i)

    for edge in tr.edges_in:
        if "guard" in edge.config:
            builder.block_begin()
            builder.line("size_t size = {0}->place_{1.id}.size();",  n, edge.place)
            builder.line("if (!({0})) {1}", edge.config["guard"], fail_command)
            builder.block_end()

        for inscription in edge.get_token_inscriptions():
            if inscription.uid in sources_uid:
                continue
            token_var = build.get_safe_id("token_{0}".format(inscription.uid))
            builder.line("if ({1}->value != ({0})) {2}",
                inscription.expr, token_var, fail_command)

    if tr.guard is not None:
        builder.line("if (!({0})) {1}", tr.guard, fail_command)

    builder.block_begin()
    builder.add_writer(fire_code)
    builder.block_end()

    """
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
        builder.line("if (n->place_{0.id}.size() < {1}) {2}", place, count, fail_command)

    em.set_extern("fail", fail_command)
    for i in initcode:
        i.emit(em, builder)

    for i, (edge, instrs) in enumerate(matches):
        builder.line("// Edge id={0.id} uid={0.uid} expr={0.expr}", edge)
        place_t = builder.emit_type(edge.get_place_type())
        place_id = edge.get_place().id
        token = "token_{0.uid}".format(edge)
        builder.line("ca::Token<{0} > *{1} = n->place_{2}.begin();", place_t, token, place_id)
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
            builder.line(fail_command)
        builder.block_end()

    builder.block_begin()
    builder.add_writer(fire_code)
    builder.block_end()

    # End of matching cycles
    for edge, _ in reversed(list(matches)):
        builder.line("token_{0.uid} = token_{0.uid}->next;", edge)
        builder.do_end("token_{0.uid} != n->place_{1.id}.begin()".format(edge, edge.get_place()))
    """

def write_core(builder):
    build.write_basic_definitions(builder)
    for net in builder.project.nets:
        write_net_functions_forward(builder, net)
    for net in builder.project.nets:
        write_net_class(builder, net)
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_place_add(builder,
                    place,
                    net_code,
                    value_code,
                    bulk=False,
                    token=False,
                    set_origin=True,
                    origin=None):
    if not bulk:
        if token:
            method = "add_token"
        else:
            method = "add"
    else:
        method = "overtake"

    if place.tracing:
        builder.if_begin("thread->get_tracelog()")
        builder.line("void (*fncs[{0}])(ca::TraceLog *, const {1} &);",
                     len(place.tracing),
                     builder.emit_type(place.type))
        for i, trace in enumerate(place.tracing):
            builder.line("fncs[{0}] = trace_{1};", i, trace.replace("fn: ", ""))
        builder.line("{0}place_{1.id}.{2}({3}, thread->get_tracelog(), {3.id}, {4}, fncs);",
                    net_code, place, method, value_code, place, len(place.tracing))
        builder.line("}} else {{")
        builder.line("{0}place_{1.id}.{2}({3});", net_code, place, method, value_code)
        builder.block_end()
    else:
        builder.line("{0}place_{1.id}.{2}({3});", net_code, place, method, value_code)

    if place.need_origin() and set_origin:
        if origin is None:
            origin = "thread->get_process_id()"
        if bulk:
            builder.line("for (int i = 0; i < {0}.size(); i++)", value_code)
            builder.block_begin()
            builder.line("{0}place_{1.id}_origin.push_back({2});", net_code, place, origin)
            builder.block_end()
        else:
            builder.line("{0}place_{1.id}_origin.push_back({2});", net_code, place, origin)

def write_init_net(builder, net):
    builder.line("ca::Context ctx(thread, net);")
    builder.line("int pid = thread->get_process_id();")
    for area in net.areas:
        if area.init_type == "vector":
            builder.line("std::vector<int> area_{0.id} = {0.init_value};", area)
        elif area.init_type == "exprs":
            builder.line("std::vector<int> area_{0.id};", area)
            for expr in area.init_value:
                builder.line("area_{0.id}.push_back({1});", area, expr)

    for place in net.places:
        if not (place.init_type or place.code):
            continue
        areas = place.get_areas()
        if areas == []:
            builder.if_begin("pid == 0")
        else:
            conditions = [ "std::find(area_{0.id}.begin(), area_{0.id}.end(), pid) !="
                           " area_{0.id}.end()"
                          .format(area) for area in areas ]
            builder.if_begin(" && ".join(conditions))

        if place.init_type == "exprs":
            for expr in place.init_value:
                write_place_add(builder, place, "net->", expr, set_origin=False)
        elif place.init_type == "vector":
            write_place_add(builder, place, "net->", place.init_value, set_origin=False)

        if place.code is not None:
            builder.line("place_user_fn_{0.id}(ctx, net->place_{0.id});", place)
            if place.need_origin():
                builder.line("for (int i = 0; i < net->place_{0.id}.size(); i++)", place)
                builder.block_begin()
                builder.line("net->place_{0.id}_origin.push_back(ctx.process_id());", place)
                builder.block_end()
        builder.block_end()

def write_spawn(builder, net):
    builder.line("ca::NetBase * spawn_{0.id}(ca::ThreadBase *thread, ca::NetDef *def) {{", net)
    builder.indent_push()
    builder.line("{0} *net = new {0}(def, ({1}*) thread);", get_net_class_name(net),
                                                            builder.thread_class)
    write_init_net(builder, net)
    builder.line("return net;")
    builder.block_end()

def write_reports_method(builder, net):
    builder.write_method_start("void write_reports_content(ca::Thread *thread, ca::Output &output)")
    for place in net.places:
        builder.line('output.child("place");')
        builder.line('output.set("id", {0.id});', place)
        builder.block_begin()
        if place.need_origin():
            builder.line("int i = 0;")
        builder.line('ca::Token<{1} > *t = place_{0.id}.begin();',
                     place, place.type)
        builder.if_begin("t")

        builder.do_begin()
        builder.line('output.child("token");')
        builder.line('output.set("value", ca::token_name(t->value));')
        if place.need_origin():
            builder.line('output.set("origin", ca::to_string(place_{0.id}_origin[i]));',
                         place)
            builder.line('i++;')
        builder.line('output.back();')
        builder.line("t = t->next;")
        builder.do_end("t != place_{0.id}.begin()".format(place))
        builder.block_end()
        builder.block_end()
        builder.line('output.back();')
    builder.write_method_end()

def write_receive_method(builder, net):
    builder.write_method_start(
        "void receive(ca::ThreadBase *thread, int from_process, int place_pos, ca::Unpacker &unpacker)")
    builder.line("switch(place_pos) {{")
    for place in net.places:
        if place.is_receiver():
            builder.line("case {0}:", place.get_pos_id())
            builder.indent_push()
            write_place_add(builder,
                            place,
                            "this->",
                            "ca::unpack<{0} >(unpacker)".format(place.type, "unpacker"),
                            origin="from_process")
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

def write_net_class(builder, net, base_class_name="ca::Net", write_class_end=True):
    builder.write_class_head(get_net_class_name(net), base_class_name)

    decls = [("def", "ca::NetDef *"),
             ("thread", "ca::Thread *")]

    builder.write_constructor(get_net_class_name(net),
                              emit_declarations(decls),
                              ["{0}(def, thread)".format(base_class_name)])
    builder.write_method_end()

    for place in net.places:
        builder.write_var_decl("place_" + str(place.id),
                               "ca::TokenList<{0} >".format(place.type))
        if place.need_origin():
            builder.write_var_decl("place_{0}_origin".format(place.id),
                                   "std::deque<int>")

    write_reports_method(builder, net)
    write_receive_method(builder, net)

    if write_class_end:
        builder.write_class_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        write_transition_functions(builder, tr)

def write_main_setup(builder, init_function="ca::init"):
    builder.line("ca::project_description({0});",
        const_string(builder.project.description))
    builder.line("std::vector<ca::Parameter*> parameters;")
    for p in builder.project.get_parameters():
        builder.line("parameters.push_back(&param::{0});", p.get_name())

    builder.emptyline()
    builder.line("{0}(argc, argv, parameters);", init_function)
    builder.emptyline()

    for net in builder.project.nets:
        write_register_net(builder, net)

    defs = [ "def_{0.id}".format(net) for net in builder.project.nets ]
    builder.line("ca::NetDef *defs[] = {{{0}}};", ",".join(defs))
    builder.line("ca::setup({0}, defs);", len(defs));
