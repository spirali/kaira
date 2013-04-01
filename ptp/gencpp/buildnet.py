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
        if inscription.is_origin_reader():
            builder.line("int origin_{0};", inscription.uid)

    for edge in tr.get_bulk_edges_in():
        builder.line("ca::TokenList<{0} > tokens_{1.uid};",
            edge.get_place_type(), edge)
        if edge.is_origin_reader():
            builder.line("std::vector<int> origins_{0.uid};", edge)

    builder.write_class_end()

def write_transition_forward(builder, tr):
    if tr.code is not None:
        write_vars_struct(builder, tr)

    write_tokens_struct(builder, tr)

    class_name = "Transition_{0.id}".format(tr)
    builder.write_class_head(class_name, "ca::TransitionDef")
    builder.write_constructor(class_name,
                              "",
                              [ "ca::TransitionDef({0}, {1})".format(
                                    tr.id, const_boolean(not tr.has_code())) ])
    builder.write_method_end()
    builder.line("ca::FireResult full_fire(ca::ThreadBase *thread, ca::NetBase *net);")
    builder.line("void* fire_phase1(ca::ThreadBase *thread, ca::NetBase *net);")
    builder.line("void fire_phase2(ca::ThreadBase *thread, ca::NetBase *net, void *data);")
    builder.line("void cleanup_binding(void *data);")
    builder.line("bool is_enable(ca::ThreadBase *thread, ca::NetBase *net);")
    if builder.generate_all_pack:
        builder.line("void pack_binding(ca::Packer &packer, void *data);")
    builder.write_class_end()
    builder.line("static Transition_{0.id} transition_{0.id};",
        tr, const_boolean(not tr.has_code()))
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
    if builder.generate_all_pack:
        write_pack_binding(builder, tr)

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
        builder.if_begin("!$lock")
        builder.line("{0}->lock();", net_expr)
        builder.line("$lock = true;")
        builder.block_end()

    def write_unlock():
        if not locking:
            return
        builder.if_begin("$lock")
        builder.line("{0}->unlock();", net_expr)
        builder.line("$lock = false;")
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
                    write_add(builder.expand("$token_{0}",
                                            reuse_tokens[inscription.uid]),
                              token=True)
                else:
                    write_add(inscription.expr)
    else: # Remote send
        if edge.is_unicast():
            sendtype = ""
            builder.if_begin(builder.expand("{0} == $thread->get_process_id()",
                                            edge.target))
            write_lock()
            if edge.is_bulk_edge():
                write_add(edge.inscriptions[0].expr, bulk=True)
            else:
                for inscription in edge.get_token_inscriptions():
                    write_add(inscription.expr)
            builder.indent_pop()
            builder.line("}} else {{")
            builder.line("int $target = {0};", edge.target)
            target = builder.expand("$target")
            builder.indent_push()
        else:
            sendtype = "_multicast"
            target = edge.target
            builder.block_begin()
        write_unlock()
        if trace_send:
            builder.if_begin("$tracelog")
            builder.line("$tracelog->event_send_part1();", target, edge)
            builder.block_end()

        if edge.is_token_edge(): # Pack token edge
            builder.line("ca::Packer $packer(ca::PACKER_DEFAULT_SIZE, ca::RESERVED_PREFIX);")
            for inscription in edge.get_token_inscriptions():
                builder.line("ca::pack($packer, {0});", inscription.expr)
            builder.line("$thread->send{0}({4}, {3}, {1}, {2}, $packer);",
                         sendtype,
                         edge.id,
                         len(edge.inscriptions),
                         net_expr,
                         target)

        else: # Bulk edge
            # TODO: Pack in one step if type is directly packable
            expr = edge.inscriptions[0].expr
            builder.line("ca::Packer $packer(ca::PACKER_DEFAULT_SIZE, ca::RESERVED_PREFIX);")
            builder.line("{0}.pack_tokens($packer);", expr);
            builder.line("$thread->send{0}({1}, {3}, {2}, ({4}).size(), $packer);",
                   sendtype, target, edge.id, net_expr, expr)
        if trace_send:
            builder.if_begin("$tracelog")
            builder.line("$tracelog->event_send_part2({0}, $packer.get_size(), {1.id});", target, edge)
            builder.block_end()
        builder.block_end()

def write_remove_tokens(builder, net_expr, tr):
    builder.block_begin()
    if tr.is_any_place_traced():
        builder.line("ca::TraceLog *$tracelog = $thread->get_tracelog();")
    for inscription in tr.get_token_inscriptions_in():
        token_var = builder.expand("$token_{0.uid}", inscription)
        place = inscription.edge.place
        if place.tracing:
            builder.if_begin("$tracelog")
            write_trace_token(builder, place, token_var, remove=True)
            builder.block_end()
        builder.line("{0}->place_{2.id}.remove({1});",
            net_expr, token_var, place)
    builder.block_end()
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

    if tr.need_trace():
        builder.line("ca::TraceLog *$tracelog = $thread->get_tracelog();")
        builder.if_begin("$tracelog")
        builder.line("$tracelog->event_transition_fired({0.id});", tr)
        builder.block_end()

    if remove_tokens:
        write_remove_tokens(builder, builder.expand("$n"), tr)

    builder.line("$n->activate_transition_by_pos_id({0});", tr.get_pos_id())
    if packed_tokens_from_place:
        for edge in tr.get_bulk_edges_in():
                place = edge.place
                if place.tracing:
                    builder.if_begin("$tracelog")
                    write_trace_token_list(builder,
                                           place,
                                           builder.expand("$n->place_{0.id}", place),
                                           remove=True)
                    builder.block_end()
                builder.line("{1} {2}($n->place_{0.id}, true);",
                    place, edge.get_type(), edge.inscriptions[0].expr)
    else:
       for edge in tr.get_bulk_edges_in():
           builder.line("{2} {3}($tokens->tokens_{0.uid}, false);",
               edge, edge.place, edge.get_type(), edge.inscriptions[0].expr)

    decls = tr.get_decls()

    for uid, t in tr.fresh_tokens:
        builder.line("ca::Token <{0} > *$token_{1} = new ca::Token<{0} >;",
                     t,
                     uid)


    for name, uid in tr.variable_sources_out.items():
        if uid is not None:
            builder.line("{0} &{1} = $token_{2}->value;", decls[name], name, uid)
        else:
            builder.line("{0} {1}; // Fresh variable", decls[name], name)


    if tr.code is not None:
        if locking:
            builder.line("$n->unlock();")
        decls = tr.get_decls().get_list()
        if len(decls) == 0:
            builder.line("Vars_{0.id} vars;", tr)
        else:
            builder.line("Vars_{0.id} vars({1});",
                tr, ",".join([ name for name, t in decls ]))
        builder.line("transition_user_fn_{0.id}(ctx, vars);", tr)
        if locking:
            builder.line("bool $lock = false;")
        if tr.need_trace():
            builder.if_begin("$tracelog")
            builder.line("$tracelog->event_transition_finished_begin();")
            builder.block_end()
    elif locking:
        builder.line("bool $lock = true;")

    if readonly_tokens:
        reuse_tokens = None
    else:
        reuse_tokens = tr.reuse_tokens;

    for edge in tr.edges_out:
        write_send_token(builder,
                         edge,
                         builder.expand("$n"),
                         trace_send=tr.need_trace(),
                         locking=locking,
                         readonly_tokens=readonly_tokens,
                         reuse_tokens=reuse_tokens)
    if locking:
        builder.line("if ($lock) $n->unlock();")

    if not readonly_tokens:
        for edge in tr.edges_in:
            for inscription in edge.get_token_inscriptions():
                if inscription.uid not in tr.reuse_tokens.values():
                    builder.line("delete $token_{0.uid};", inscription)

    if tr.need_trace():
        builder.if_begin("$tracelog")
        builder.line("$tracelog->event_end();")
        builder.block_end()

def write_full_fire(builder, tr, locking=True):
    builder.line("ca::FireResult Transition_{0.id}::full_fire(ca::ThreadBase *$thread, ca::NetBase *$net)",
                 tr)
    builder.block_begin()
    builder.line("ca::Context ctx($thread, $net);")

    w = build.Builder(builder.project)
    write_fire_body(w, tr, locking=locking)
    w.line("return ca::TRANSITION_FIRED;")

    write_enable_pattern_match(builder, tr, w, "return ca::NOT_ENABLED;")
    builder.line("return ca::NOT_ENABLED;")
    builder.block_end()

def write_enable_check(builder, tr):
    builder.line("bool Transition_{0.id}::is_enable(ca::ThreadBase *$thread, ca::NetBase *$net)",
                 tr)
    builder.block_begin()
    builder.line("ca::Context ctx($thread, $net);")
    w = CppWriter()
    w.line("return true;")
    write_enable_pattern_match(builder, tr, w, "return false;")
    builder.line("return false;")
    builder.block_end()

def write_fire_phase1(builder, tr):
    builder.line("void *Transition_{0.id}::fire_phase1(ca::ThreadBase *$thread, ca::NetBase *$net)", tr)
    builder.block_begin()
    builder.line("ca::Context ctx($thread, $net);")

    # ---- Prepare builder --- #
    w = build.Builder(builder.project)
    if tr.need_trace():
        builder.line("ca::TraceLog *$tracelog = $thread->get_tracelog();")
    write_remove_tokens(w, builder.expand("$n"), tr)
    w.line("Tokens_{0.id} *$tokens = new Tokens_{0.id}();", tr)
    for inscription in tr.get_token_inscriptions_in():
        w.line("$tokens->token_{0.uid} = $token_{0.uid};", inscription);
        if inscription.is_origin_reader():
            w.line("$tokens->origin_{0.uid} = {0.expr}_origin;", inscription)

    for edge in tr.get_bulk_edges_in():
        w.line("$tokens->tokens_{0.uid}.overtake($n->place_{1.id});",
               edge, edge.place)

    for edge in tr.get_bulk_edges_in():
        if edge.is_origin_reader():
            w.line("$tokens->origins_{0.uid} = {1.expr}_origins;",
                edge, edge.inscriptions[0])

    w.line("return $tokens;")
    # --- End of prepare --- #

    write_enable_pattern_match(builder, tr, w, "return NULL;")

    builder.line("return NULL;")
    builder.block_end()

def write_fire_phase2(builder, tr):
    builder.line("void Transition_{0.id}::fire_phase2"
                    "(ca::ThreadBase *$thread, ca::NetBase *$net, void *$data)",
                 tr)
    builder.block_begin()

    builder.line("ca::Context ctx($thread, $net);")
    builder.line("{0} *$n = ({0}*) $net;", get_net_class_name(tr.net))
    builder.line("Tokens_{0.id} *$tokens = (Tokens_{0.id}*) $data;", tr)

    decls_dict = tr.get_decls()

    for name, uid in tr.variable_sources.items():
        if uid is not None:
            builder.line(
                "{0} {1} = $tokens->token_{2}->value;", decls_dict[name], name, uid)

    for inscription in tr.get_token_inscriptions_in():
        if inscription.is_origin_reader():
            builder.line("int {0.expr}_origin = $tokens->origin_{0.uid};", inscription)

    for edge in tr.get_bulk_edges_in():
        if edge.is_origin_reader():
            builder.line("std::vector<int> {1.expr}_origins = $tokens->origins_{0.uid};",
                edge, edge.inscriptions[0])

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
    for inscription in tr.get_token_inscriptions_in():
        builder.line("delete tokens->token_{1.uid};",
            inscription.get_type(), inscription);
    builder.line("delete tokens;");
    builder.block_end()

def write_pack_binding(builder, tr):
    builder.line("void Transition_{0.id}::pack_binding(ca::Packer &packer, void *data)", tr)
    builder.block_begin()
    builder.line("Tokens_{0.id} *tokens = (Tokens_{0.id}*) data;", tr)

    for inscription in tr.get_token_inscriptions_in():
        builder.line("ca::pack(packer, tokens->token_{0}->value);", inscription.uid);
        if inscription.is_origin_reader():
            builder.line("ca::pack(packer, tokens->origin_{0});", inscription.uid);

    for edge in tr.get_bulk_edges_in():
        builder.line("ca::pack(packer, tokens->tokens_{0.uid});", edge);
        if edge.is_origin_reader():
            builder.line("ca::pack(packer, tokens->origins_{0.uid});", edge);


    builder.block_end()

def write_enable_pattern_match(builder, tr, fire_code, fail_command):
    builder.line("{0} *$n = ({0}*) $net;", get_net_class_name(tr.net))

    # Check if there are enough tokens
    for edge in tr.edges_in:
        if edge.get_tokens_number() is not None:
            builder.line("if ($n->place_{0.id}.size() < {1}) {2}",
                edge.place, edge.get_tokens_number(), fail_command)

    for edge in tr.edges_in:
        prev_uid = None
        for inscription in edge.get_token_inscriptions():
            builder.line("// Edge id={0.id} uid={1.uid} expr={1.expr}",
                edge, inscription)
            line = builder.expand("ca::Token < {0} > *$token_{1.uid} = ",
                        edge.get_place_type(),
                        inscription)
            if prev_uid is None:
                builder.line("{0} $n->place_{1.id}.begin();",
                            line, edge.place)
            else:
                builder.line("{0} $token_{1}->next;", line, prev_uid)
            prev_uid = inscription.uid

    sources_uid = [ uid for uid in tr.variable_sources.values() if uid is not None ]
    decls_dict = tr.get_decls()

    for name, uid in tr.variable_sources.items():
        if uid is not None:
            builder.line("{0} &{1} = $token_{2}->value;", decls_dict[name], name, uid)

    for edge in tr.edges_in:
        if edge.is_origin_reader():
            if edge.is_bulk_edge():
              builder.line("std::vector<int> {0}_origins($n->place_{1.id}_origin.begin(),"
                           " $n->place_{1.id}_origin.end());",
                           edge.inscriptions[0].expr, edge.place)

            else:
                for i, inscription in enumerate(edge.inscriptions):
                    builder.line("int {0}_origin = *($n->place_{1.id}_origin.begin() + {2});",
                                 inscription.expr, edge.place, i)

    for edge in tr.edges_in:
        if "guard" in edge.config:
            builder.block_begin()
            builder.line("size_t size = $n->place_{0.id}.size();", edge.place)
            builder.line("if (!({0})) {1}", edge.config["guard"], fail_command)
            builder.block_end()

        for inscription in edge.get_token_inscriptions():
            if inscription.uid in sources_uid:
                continue

            builder.line("if ($token_{2.uid}->value != ({0})) {1}",
                inscription.expr, fail_command, inscription)

    if tr.guard is not None:
        builder.line("if (!({0})) {1}", tr.guard, fail_command)

    builder.block_begin()
    builder.add_writer(fire_code)
    builder.block_end()

def write_core(builder):
    build.write_basic_definitions(builder)
    for net in builder.project.nets:
        write_net_functions_forward(builder, net)
    for net in builder.project.nets:
        write_net_class(builder, net)
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_trace_token(builder, place, token_code, remove=False):
    if remove:
        builder.line("$tracelog->trace_token_remove({0.id}, {1});", place, token_code)
    else:
        builder.line("$tracelog->trace_token_add({0.id}, {1});", place, token_code)
        for name, return_type in place.tracing:
            builder.line("$tracelog->trace_value({1}({0}->value));", token_code, name)

def write_trace_token_list(builder, place, token_list, remove=False, begin=None):
    if begin is None:
        begin = "{0}.begin()".format(token_list)

    builder.line("for (ca::Token<{1.type} > *token={2};"
                 "token != NULL;"
                 "token = {0}.next(token))",
                 token_list, place, begin)
    builder.block_begin()
    write_trace_token(builder, place, "token", remove)
    builder.block_end()

def write_place_add(builder,
                    place,
                    net_code,
                    value_code,
                    bulk=False,
                    token=False,
                    set_origin=True,
                    trace=True,
                    origin=None):
    if not bulk:
        if token:
            method = "add_token"
        else:
            method = "add"
    else:
        method = "overtake"

    if place.need_origin() and set_origin:
        if origin is None:
            origin = builder.expand("$thread->get_process_id()")
        if bulk:
            builder.line("for (int i = 0; i < {0}.size(); i++)", value_code)
            builder.block_begin()
            builder.line("{0}place_{1.id}_origin.push_back({2});", net_code, place, origin)
            builder.block_end()
        else:
            builder.line("{0}place_{1.id}_origin.push_back({2});", net_code, place, origin)


    if trace and place.tracing and bulk:
        builder.if_begin("$thread->get_tracelog()")
        builder.line("ca::TraceLog *tracelog = $thread->get_tracelog();")
        write_trace_token_list(builder, place, value_code)
        builder.block_end()

    builder.line("{0}place_{1.id}.{2}({3});", net_code, place, method, value_code)

    if trace and place.tracing and not bulk:
        builder.if_begin("$thread->get_tracelog()")
        builder.line("ca::TraceLog *$tracelog = $thread->get_tracelog();")
        builder.line("ca::Token<{1.type} > *$token = {0}place_{1.id}.last();", net_code, place)
        write_trace_token(builder, place, builder.expand("$token"))
        builder.block_end()

def write_init_net(builder, net):
    builder.line("ca::Context ctx($thread, $net);")
    builder.line("int $pid = $thread->get_process_id();")
    for area in net.areas:
        if area.init_type == "vector":
            builder.line("std::vector<int> $area_{0.id} = {0.init_value};", area)
        elif area.init_type == "exprs":
            builder.line("std::vector<int> $area_{0.id};", area)
            for expr in area.init_value:
                builder.line("$area_{0.id}.push_back({1});", area, expr)

    for place in net.places:
        if not (place.init_type or place.code):
            continue
        areas = place.get_areas()
        if areas == []:
            builder.if_begin("$pid == 0")
        else:
            conditions = [ builder.expand("std::find($area_{0.id}.begin(), $area_{0.id}.end(), $pid) !="
                           " $area_{0.id}.end()", area) for area in areas ]
            builder.if_begin(" && ".join(conditions))

        if place.init_type == "exprs":
            for expr in place.init_value:
                write_place_add(builder,
                                place,
                                builder.expand("$net->"),
                                expr,
                                set_origin=False,
                                trace=False)
        elif place.init_type == "vector":
            write_place_add(builder,
                            place,
                            builder.expand("$net->"),
                            place.init_value,
                            set_origin=False,
                            trace=False)

        if place.code is not None:
            builder.line("place_user_fn_{0.id}(ctx, $net->place_{0.id});", place)

        if place.tracing:
            builder.if_begin("$thread->get_tracelog()")
            builder.line("ca::TraceLog *$tracelog = $thread->get_tracelog();")
            write_trace_token_list(builder,
                                   place,
                                   builder.expand("$net->place_{0.id}", place))
            builder.block_end()

        if place.need_origin():
            builder.line("for (int i = 0; i < $net->place_{0.id}.size(); i++)", place)
            builder.block_begin()
            builder.line("$net->place_{0.id}_origin.push_back(ctx.process_id());", place)
            builder.block_end()
        builder.block_end()

def write_spawn(builder, net):
    builder.line("ca::NetBase * spawn_{0.id}(ca::ThreadBase *$thread, ca::NetDef *$def) {{", net)
    builder.indent_push()
    builder.line("{0} *$net = new {0}($def, ({1}*) $thread);", get_net_class_name(net),
                                                            builder.thread_class)
    write_init_net(builder, net)
    builder.line("return $net;")
    builder.block_end()

def write_reports_method(builder, net):
    builder.write_method_start("void write_reports_content(ca::ThreadBase *thread, ca::Output &output)")
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
        "void receive(ca::ThreadBase *$thread, int from_process, int place_pos, ca::Unpacker &unpacker)")
    builder.line("switch(place_pos) {{")
    for edge in net.get_edges_out():
        if not edge.is_local():
            builder.line("case {0}:", edge.id)
            builder.indent_push()
            write_place_add(builder,
                            edge.place,
                            "this->",
                            "ca::unpack<{0} >(unpacker)".format(edge.place.type, "unpacker"),
                            origin="from_process")
            write_activation(builder, "this", edge.place.get_transitions_out())
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

def write_net_class(builder,
                    net,
                    namespace="ca",
                    write_constructor=True,
                    write_class_end=True):

    builder.write_class_head(get_net_class_name(net), namespace + "::Net")

    decls = [("def", "ca::NetDef *"),
             ("thread", "ca::Thread *")]

    if write_constructor:
        builder.write_constructor(get_net_class_name(net),
                                  emit_declarations(decls),
                                  ["{0}::Net(def, thread)".format(namespace)])
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
