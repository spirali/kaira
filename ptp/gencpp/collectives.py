#
#    Copyright (C) 2014 Stanislav Bohm
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
#    along with Kaira.  If not, see <http://wwbuilder.gnu.org/licenses/>.
#

import buildnet
import parser

def write_scatter_root(builder, tr, inscription):
    builder.line("const std::vector<{0.type} > &$value = {1};",
                 inscription.edge.place,
                 inscription.expr)

    # Check size of vector
    builder.if_begin("$value.size() != $thread->get_process_count()")
    builder.line("fprintf(stderr, \"[scatter] Invalid size of vector (expected=%i, got=%lu)\\n\","
                    "$thread->get_process_count(), $value.size());")
    builder.line("exit(1);")
    builder.block_end()

    builder.line("const size_t $size = sizeof({0.type});", inscription)
    # builder.line("ca::Packer $packer($size);")
    # builder.line("ca::pack_with_step($packer, $value, $size);")
    builder.line("$thread->collective_scatter_root({0.id}, &$value[0], $size);", tr)
    # builder.line("$packer.free();")
    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$value[$root]"),
                    bulk=False,
                    token=False)
    buildnet.write_activation(builder, builder.expand("$n"), inscription.edge.place.get_transitions_out())

def write_scatter_nonroot(builder, tr, inscription):
    builder.line("const size_t $size = sizeof({0.type});", inscription)
    builder.line("ca::Token<{0.type} > *$token = new ca::Token<{0.type} >;",
            inscription.edge.place)
    builder.line("$thread->collective_scatter_nonroot({0.id}, $root, &$token->value, $size);", tr)
    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)
    buildnet.write_activation(builder, builder.expand("$n"), inscription.edge.place.get_transitions_out())

def write_scatter_body_simulation(builder, tr, inscription, readonly):
    if readonly:
        buildnet.write_place_add(builder,
                        inscription.edge.place,
                        builder.expand("$n->"),
                        builder.expand("$tokens->token_collective"),
                        bulk=False,
                        token=True)
    else:
        buildnet.write_place_add(builder,
                        inscription.edge.place,
                        builder.expand("$n->"),
                        builder.expand("$tokens->token_collective->value"),
                        bulk=False,
                        token=False)

def write_phase1_scatter_preinit(builder, tr, inscription):
    builder.line("Tokens_{0.id} *$rbinding = static_cast<Tokens_{0.id}*>($bindings[$root]);", tr)
    buildnet.write_unpack_binding(builder, tr, builder.expand("$rbinding"))
    builder.line("const std::vector<{0.type} > &$ccdata = {0.expr};", inscription)
    builder.if_begin("$ccdata.size() != $thread->get_process_count()")
    builder.line("fprintf(stderr, \"Invalid number of scattered elements (%lu)\\n\","
                 "$ccdata.size());")
    builder.line("exit(1);")
    builder.block_end()

def write_phase1_scatter_forall(builder, tr, inscription):
    builder.line("$t->token_collective = new ca::Token<{0.type} >($ccdata[$i]);", inscription)

def write_gather_root(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("const size_t $size = sizeof({0});", typename)
    builder.line("ca::Token<std::vector<{0} > > *$token = new ca::Token<std::vector<{0} > >;", typename)
    builder.line("$token->value.resize($thread->get_process_count());")
    builder.line("$thread->collective_gather_root({0.id}, &{1.expr}, $size, &$token->value[0]);",
                 tr, inscription)

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)

def write_gather_nonroot(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("const size_t $size = sizeof({0});", typename)
    builder.line("$thread->collective_gather_nonroot({0.id}, $root, &{1.expr}, $size);",
                 tr, inscription)

def write_gather_body_simulation(builder, tr, inscription, readonly):
    builder.if_begin("$root == $thread->get_process_id()")
    if readonly:
        buildnet.write_place_add(builder,
                        inscription.edge.place,
                        builder.expand("$n->"),
                        builder.expand("$tokens->token_collective"),
                        bulk=False,
                        token=True)
    else:
        buildnet.write_place_add(builder,
                        inscription.edge.place,
                        builder.expand("$n->"),
                        builder.expand("$tokens->token_collective->value"),
                        bulk=False,
                        token=False)
    builder.block_end()

def write_phase1_gather_preinit(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("const size_t $size = sizeof({0});", typename)
    builder.line("Tokens_{0.id} *$rbinding = static_cast<Tokens_{0.id}*>($bindings[$root]);", tr)
    builder.line("$rbinding->token_collective = new ca::Token<{0.type} >;", inscription)
    builder.line("$rbinding->token_collective->value.resize($thread->get_process_count());")

def write_phase1_gather_forall(builder, tr, inscription):
    buildnet.write_unpack_binding(builder,
                                  tr,
                                  builder.expand("static_cast<Tokens_{0.id}*>($bindings[$i])", tr))
    builder.line("$rbinding->token_collective->value[$i] = {0.expr};", inscription)

def write_collective_body(builder, tr):
    inscription = tr.get_collective_inscription()
    op = inscription.get_collective_operation()
    builder.if_begin("$root == $thread->get_process_id()")

    if op == "scatter":
        write_scatter_root(builder, tr, inscription)
    elif op == "gather":
        write_gather_root(builder, tr, inscription)

    builder.write_else()

    if op == "scatter":
        write_scatter_nonroot(builder, tr, inscription)
    elif op == "gather":
        write_gather_nonroot(builder, tr, inscription)

    builder.block_end()

def write_collective_body_simulation(builder, tr, readonly):
    inscription = tr.get_collective_inscription()
    op = inscription.get_collective_operation()
    if op == "scatter":
        write_scatter_body_simulation(builder, tr, inscription, readonly)
    elif op == "gather":
        write_gather_body_simulation(builder, tr, inscription, readonly)

def write_collective_phase1(builder, tr):
    builder.line("$tokens->blocked = true;")
    builder.line("$tokens->root = $root;")
    builder.line("$tokens->token_collective = NULL;")
    builder.line("std::vector<void*> $bindings;")
    builder.line("int $bcount = $thread->collective_bindings(this, $bindings);")

    # Check roots
    builder.for_begin("int $i = 0; $i < $thread->get_process_count(); $i++")
    builder.line("Tokens_{0.id} *$t = static_cast<Tokens_{0.id}*>($bindings[$i]);", tr)
    builder.if_begin("$t && $t->root != $root")
    builder.line("fprintf(stderr, \"Collective transition started with different roots; "
           "root=%i at process %i and root=%i at process %i\\n\","
           "$root, $thread->get_process_id(), $t->root, $i);")
    builder.line("exit(1);")
    builder.block_end()
    builder.block_end()

    # Perform collective operation
    builder.if_begin("$bcount == $thread->get_process_count() - 1")
    builder.line("$bindings[$thread->get_process_id()] = $tokens;")
    inscription = tr.get_collective_inscription()
    op = inscription.get_collective_operation()
    if op == "scatter":
        write_phase1_scatter_preinit(builder, tr, inscription)
    else:
        write_phase1_gather_preinit(builder, tr, inscription)
    builder.for_begin("int $i = 0; $i < $thread->get_process_count(); $i++")
    builder.line("Tokens_{0.id} *$t = static_cast<Tokens_{0.id}*>($bindings[$i]);", tr)
    builder.line("$t->blocked = false;")
    if op == "scatter":
        write_phase1_scatter_forall(builder, tr, inscription)
    elif op == "gather":
        write_phase1_gather_forall(builder, tr, inscription)
    builder.block_end()

    builder.block_end()
