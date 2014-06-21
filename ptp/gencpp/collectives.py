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

def write_pack_fixed_size(builder, typename, expr):
    builder.line("const size_t $size = ca::fixed_size<{0} >();", typename)
    builder.line("ca::Packer $packer($size);")
    builder.line("$packer << {0};", expr)
    builder.if_begin("$packer.get_size() > $size")
    builder.line("fprintf(stderr, \"Type '{0}' has fixed size %lu,"
                 "but %lu bytes was packed.\\n\", $size, $packer.get_size());", typename)
    builder.line("exit(1);")
    builder.block_end()

# Scatter ----------------------------------------------------------------------------------------

def write_scatter_root(builder, tr, inscription):
    builder.line("const std::vector<{0.type} > &$value = {1};",
                 inscription.edge.place,
                 inscription.expr)

    # Check size of vector
    builder.line("int $process_count = $thread->get_process_count();")
    builder.if_begin("$value.size() != $process_count")
    builder.line("fprintf(stderr, \"[scatter] Invalid size of vector (expected=%i, got=%lu)\\n\","
                    "$process_count, $value.size());")
    builder.line("exit(1);")
    builder.block_end()

    builder.if_begin("ca::is_trivially_packable<{0}>()", inscription.type)
    # Trivially packable
    builder.line("$thread->collective_scatter_root({0.id}, &$value[0], sizeof({1.type}));",
        tr, inscription)
    builder.else_if("ca::fixed_size<{0} >() != 0", inscription.type)
    # Fixed size
    builder.line("size_t $size = ca::fixed_size<{0} >();", inscription.type)
    builder.line("ca::Packer $packer($size * $process_count);")
    builder.line("ca::pack_with_step($packer, $value, $size);")
    builder.line("$thread->collective_scatter_root({0.id}, $packer.get_buffer(), $size);",
        tr, inscription)
    builder.line("$packer.free();")
    builder.write_else()
    # Generic case
    builder.line("int $process_id = $thread->get_process_id();")
    builder.line("ca::Packer $packer;")
    builder.line("int *$sizes = static_cast<int*>(alloca(sizeof(int) * $process_count));")
    builder.line("int *$displs = static_cast<int*>(alloca(sizeof(int) * (1 + $process_count)));")
    builder.line("$displs[0] = 0;")
    builder.for_begin("int $i = 0; $i < $process_id; $i++")
    builder.line("$packer << $value[$i];")
    builder.line("$displs[$i + 1] = $packer.get_size();")
    builder.line("$sizes[$i] = $displs[$i + 1] - $displs[$i];")
    builder.block_end()
    builder.line("$displs[$process_id + 1] = $displs[$process_id];")
    builder.line("$sizes[$process_id] = 0;")
    builder.for_begin("int $i = $process_id; $i < $process_count; $i++")
    builder.line("$packer << $value[$i];")
    builder.line("$displs[$i + 1] = $packer.get_size();")
    builder.line("$sizes[$i] = $displs[$i + 1] - $displs[$i];")
    builder.block_end()
    builder.line("$thread->collective_scatter_root({0.id}, $sizes, sizeof(int));", tr)
    builder.line("$thread->collective_scatterv_root("
                 "{0.id}, $packer.get_buffer(), $sizes, $displs);", tr)
    builder.line("$packer.free();")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$value[$root]"),
                    bulk=False,
                    token=False)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

def write_scatter_nonroot(builder, tr, inscription):
    builder.line("const size_t $size = sizeof({0.type});", inscription)
    builder.line("ca::Token<{0.type} > *$token = new ca::Token<{0.type} >;",
            inscription.edge.place)

    builder.if_begin("ca::is_trivially_packable<{0}>()", inscription.type)
    # Trivially packable
    builder.line("$thread->collective_scatter_nonroot({0.id}, $root, &$token->value, $size);", tr)
    builder.else_if("ca::fixed_size<{0} >() != 0", inscription.type)
    # Fixed size
    builder.line("size_t $size = ca::fixed_size<{0} >();", inscription.type)
    builder.line("void *$mem = malloc($size * $thread->get_process_count());")
    builder.line("$thread->collective_scatter_nonroot({0.id}, $root, $mem, $size);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("$unpacker >> $token->value;")
    builder.line("free($mem);")
    builder.write_else()
    # Generic case
    builder.line("int $size;")
    builder.line("$thread->collective_scatter_nonroot({0.id}, $root, &$size, sizeof(int));", tr)
    builder.line("void *$mem = malloc($size);")
    builder.line("$thread->collective_scatterv_nonroot({0.id}, $root, $mem, $size);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("$unpacker >> $token->value;")
    builder.line("free($mem);")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

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

# Gather --------------------------------------------------------------------

def write_gather_root(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("ca::Token<std::vector<{0} > > *$token = new ca::Token<std::vector<{0} > >;",
                 typename)

    builder.if_begin("ca::is_trivially_packable<{0} >()", typename)
    # Trivially packable
    builder.line("const size_t $size = sizeof({0});", typename)
    builder.line("$token->value.resize($thread->get_process_count());")
    builder.line("$thread->collective_gather_root({0.id}, &{1.expr}, $size, &$token->value[0]);",
                 tr, inscription)
    builder.else_if("ca::fixed_size<{0} >() != 0", typename)
    # Fixed size
    write_pack_fixed_size(builder, typename, inscription.expr)
    builder.line("void *$mem = malloc($size * $thread->get_process_count());")
    builder.line("$thread->collective_gather_root({0.id}, $packer.get_buffer(), $size, $mem);",
                 tr, inscription)
    builder.line("$packer.free();")
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("ca::unpack_with_step($unpacker,"
                 "$token->value, $size, $thread->get_process_count());")

    builder.line("free($mem);")
    builder.write_else()
    # Generic case
    builder.line("int $process_count = $thread->get_process_count();")
    builder.line("int *$sizes = static_cast<int*>(alloca(sizeof(int) * $process_count));")
    builder.line("int *$displs = static_cast<int*>(alloca(sizeof(int) * (1 + $process_count)));")
    builder.line("int $size = 0;")
    builder.line("$thread->collective_gather_root({0.id}, &$size, sizeof(int), $sizes);", tr)
    builder.line("$displs[0] = 0;")
    # Last displs[process_count] == sum of all sizes
    builder.for_begin("int $i = 0; $i < ca::process_count; $i++")
    builder.line("$displs[$i + 1] = $displs[$i] + $sizes[$i];")
    builder.block_end()
    builder.line("void *$mem = malloc($displs[$process_count]);")
    builder.line("$thread->collective_gatherv_root({0.id}, &$size, 0, $mem, $sizes, $displs);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("int $process_id = $thread->get_process_id();")
    builder.line("ca::unpack_with_displs($unpacker, $token->value, $process_id, $displs);")
    builder.line("$token->value.push_back({0});", inscription.expr);
    builder.line("ca::unpack_with_displs"
                 "($unpacker, $token->value, $process_count - $process_id - 1,"
                             "$displs + $process_id + 1);")
    builder.line("free($mem);")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

def write_gather_nonroot(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.if_begin("ca::is_trivially_packable<{0} >()", typename)
    # Trivially packable
    builder.line("$thread->collective_gather_nonroot({0.id}, $root, &{1.expr}, sizeof({2}));",
                 tr, inscription, typename)
    builder.else_if("ca::fixed_size<{0} >() != 0", typename)
    # Fixed size
    write_pack_fixed_size(builder, typename, inscription.expr)
    builder.line("$thread->collective_gather_nonroot({0.id}, $root, $packer.get_buffer(), $size);",
                 tr, inscription, typename)
    builder.line("$packer.free();")
    builder.write_else()
    # Generic case
    builder.line("ca::Packer $packer;")
    builder.line("$packer << {0};", inscription.expr)
    builder.line("int $size = $packer.get_size();")
    builder.line("$thread->collective_gather_nonroot"
                 "({0.id}, $root, &$size, sizeof(int));", tr)
    #builder.line("fprintf(stderr, \"%i %i\\n\", $thread->get_process_id(), size)");
    builder.line("$thread->collective_gatherv_nonroot"
                 "({0.id}, $root, $packer.get_buffer(), $packer.get_size());",
                 tr, inscription, typename)
    builder.line("$packer.free();")
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

# Bcast ----------------------------------------------------------

def write_bcast_root(builder, tr, inscription):
    # TODO: Reuse token when possible
    builder.line("const {0.type} &$value = {1};",
                 inscription.edge.place,
                 inscription.expr)

    builder.if_begin("ca::is_trivially_packable<{0}>()", inscription.type)
    # Trivially packable
    builder.line("$thread->collective_bcast_root({0.id}, &$value, sizeof({1.type}));",
        tr, inscription)
    builder.else_if("ca::fixed_size<{0} >() != 0", inscription.type)
    # Fixed size
    write_pack_fixed_size(builder, inscription.type, builder.expand("$value"))
    builder.line("$thread->collective_bcast_root({0.id}, $packer.get_buffer(), $size);",
        tr, inscription)
    builder.line("$packer.free();")
    builder.write_else()
    # Generic case
    builder.line("int $process_id = $thread->get_process_id();")
    builder.line("ca::Packer $packer;")
    builder.line("$packer << $value;")
    builder.line("int $size = $packer.get_size();")
    builder.line("$thread->collective_bcast_root({0.id}, &$size, sizeof(int));", tr)
    builder.line("$thread->collective_bcast_root("
                 "{0.id}, $packer.get_buffer(), $size);", tr)
    builder.line("$packer.free();")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$value"),
                    bulk=False,
                    token=False)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

def write_bcast_nonroot(builder, tr, inscription):
    builder.line("const size_t $size = sizeof({0.type});", inscription)
    builder.line("ca::Token<{0.type} > *$token = new ca::Token<{0.type} >;",
            inscription.edge.place)

    builder.if_begin("ca::is_trivially_packable<{0}>()", inscription.type)
    # Trivially packable
    builder.line("$thread->collective_bcast_nonroot({0.id}, $root, &$token->value, $size);", tr)
    builder.else_if("ca::fixed_size<{0} >() != 0", inscription.type)
    # Fixed size
    builder.line("size_t $size = ca::fixed_size<{0} >();", inscription.type)
    builder.line("void *$mem = malloc($size * $thread->get_process_count());")
    builder.line("$thread->collective_bcast_nonroot({0.id}, $root, $mem, $size);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("$unpacker >> $token->value;")
    builder.line("free($mem);")
    builder.write_else()
    # Generic case
    builder.line("int $size;")
    builder.line("$thread->collective_bcast_nonroot({0.id}, $root, &$size, sizeof(int));", tr)
    builder.line("void *$mem = malloc($size);")
    builder.line("$thread->collective_bcast_nonroot({0.id}, $root, $mem, $size);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("$unpacker >> $token->value;")
    builder.line("free($mem);")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

def write_phase1_bcast_preinit(builder, tr, inscription):
    builder.line("Tokens_{0.id} *$rbinding = static_cast<Tokens_{0.id}*>($bindings[$root]);", tr)
    buildnet.write_unpack_binding(builder, tr, builder.expand("$rbinding"))
    builder.line("const {0.type} &$ccdata = {0.expr};", inscription)

def write_phase1_bcast_forall(builder, tr, inscription):
    builder.line("$t->token_collective = new ca::Token<{0.type} >($ccdata);", inscription)

# Allgather ------------------------------------------------------------------

def write_allgather(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("ca::Token<std::vector<{0} > > *$token = new ca::Token<std::vector<{0} > >;",
                 typename)

    builder.if_begin("ca::is_trivially_packable<{0} >()", typename)
    # Trivially packable
    builder.line("const size_t $size = sizeof({0});", typename)
    builder.line("$token->value.resize($thread->get_process_count());")
    builder.line("$thread->collective_allgather({0.id}, &{1.expr}, $size, &$token->value[0]);",
                 tr, inscription)
    builder.else_if("ca::fixed_size<{0} >() != 0", typename)
    # Fixed size
    write_pack_fixed_size(builder, typename, inscription.expr)
    builder.line("void *$mem = malloc($size * $thread->get_process_count());")
    builder.line("$thread->collective_allgather({0.id}, $packer.get_buffer(), $size, $mem);",
                 tr, inscription)
    builder.line("$packer.free();")
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("ca::unpack_with_step($unpacker,"
                 "$token->value, $size, $thread->get_process_count());")

    builder.line("free($mem);")
    builder.write_else()
    # Generic case
    builder.line("int $process_count = $thread->get_process_count();")
    builder.line("int *$sizes = static_cast<int*>(alloca(sizeof(int) * $process_count));")
    builder.line("int *$displs = static_cast<int*>(alloca(sizeof(int) * (1 + $process_count)));")
    builder.line("int $size = 0;")
    builder.line("$thread->collective_allgather({0.id}, &$size, sizeof(int), $sizes);", tr)
    builder.line("$displs[0] = 0;")
    # Last displs[process_count] == sum of all sizes
    builder.for_begin("int $i = 0; $i < ca::process_count; $i++")
    builder.line("$displs[$i + 1] = $displs[$i] + $sizes[$i];")
    builder.block_end()
    builder.line("void *$mem = malloc($displs[$process_count]);")
    builder.line("$thread->collective_allgatherv({0.id}, &$size, 0, $mem, $sizes, $displs);", tr)
    builder.line("ca::Unpacker $unpacker($mem);")
    builder.line("int $process_id = $thread->get_process_id();")
    builder.line("ca::unpack_with_displs($unpacker, $token->value, $process_id, $displs);")
    builder.line("$token->value.push_back({0});", inscription.expr);
    builder.line("ca::unpack_with_displs"
                 "($unpacker, $token->value, $process_count - $process_id - 1,"
                             "$displs + $process_id + 1);")
    builder.line("free($mem);")
    builder.block_end()

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$token"),
                    bulk=False,
                    token=True)
    buildnet.write_activation(builder,
                              builder.expand("$n"),
                              inscription.edge.place.get_transitions_out())

def write_phase1_allgather_preinit(builder, tr, inscription):
    t = parser.parse_typename(inscription.type, inscription.source)
    typename = t[1][0] # This should not failed, because check already verify this
    builder.line("const size_t $size = sizeof({0});", typename)

    builder.for_begin("int $i = 0; $i < $thread->get_process_count(); $i++")
    builder.line("Tokens_{0.id} *$t = static_cast<Tokens_{0.id}*>($bindings[$i]);", tr)
    builder.line("$t->token_collective = new ca::Token<{0.type} >;", inscription)
    builder.line("$t->token_collective->value.resize($thread->get_process_count());")
    builder.block_end()

def write_phase1_allgather_forall(builder, tr, inscription):
    buildnet.write_unpack_binding(builder,
                                  tr,
                                  builder.expand("static_cast<Tokens_{0.id}*>($bindings[$i])", tr))
    builder.for_begin("int $j = 0; $j < $thread->get_process_count(); $j++")
    builder.line("Tokens_{0.id} *$t = static_cast<Tokens_{0.id}*>($bindings[$j]);", tr)
    builder.line("$t->token_collective->value[$i] = {0.expr};", inscription)
    builder.block_end()

def write_collective_body(builder, tr):
    op = tr.get_collective_operation()
    inscription = tr.get_collective_inscription()

    if not tr.root:
        if op == "barrier":
            builder.line("$thread->collective_barrier({0.id});", tr)
            return
        if op == "allgather":
            write_allgather(builder, tr, inscription)
    else:
        builder.if_begin("$root == $thread->get_process_id()")

        if op == "scatter":
            write_scatter_root(builder, tr, inscription)
        elif op == "gather":
            write_gather_root(builder, tr, inscription)
        elif op == "bcast":
            write_bcast_root(builder, tr, inscription)

        builder.write_else()

        if op == "scatter":
            write_scatter_nonroot(builder, tr, inscription)
        elif op == "gather":
            write_gather_nonroot(builder, tr, inscription)
        elif op == "bcast":
            write_bcast_nonroot(builder, tr, inscription)

        builder.block_end()

def write_collective_body_simulation(builder, tr):
    inscription = tr.get_collective_inscription()
    op = tr.get_collective_operation()
    if op == "barrier":
        return
    if op == "gather":
        builder.if_begin("$root == $thread->get_process_id()")

    buildnet.write_place_add(builder,
                    inscription.edge.place,
                    builder.expand("$n->"),
                    builder.expand("$tokens->token_collective"),
                    bulk=False,
                    token=True)
    if op == "gather":
        builder.block_end()

def write_collective_phase1(builder, tr):
    builder.line("$tokens->blocked = true;")
    if tr.root:
        builder.line("$tokens->root = $root;")
        builder.line("$tokens->token_collective = NULL;")

    builder.line("std::vector<ca::Binding*> $bindings;")
    builder.line("int $bcount = $thread->collective_bindings(this, $bindings);")
    builder.line("int $process_count = $thread->get_process_count();")

    if tr.root:
        # Check roots
        builder.if_begin("$root < 0 || $root >= $process_count")
        builder.line("fprintf(stderr, \"Collective transition started with invalid root; "
                     "root=%i at process %i \\n\","
                     "$root, $thread->get_process_id());")
        builder.line("exit(1);")
        builder.block_end()

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
    op = tr.get_collective_operation()
    if op == "scatter":
        write_phase1_scatter_preinit(builder, tr, inscription)
    elif op == "gather":
        write_phase1_gather_preinit(builder, tr, inscription)
    elif op == "bcast":
        write_phase1_bcast_preinit(builder, tr, inscription)
    elif op == "allgather":
        write_phase1_allgather_preinit(builder, tr, inscription)

    builder.for_begin("int $i = 0; $i < $thread->get_process_count(); $i++")
    builder.line("Tokens_{0.id} *$t = static_cast<Tokens_{0.id}*>($bindings[$i]);", tr)
    builder.line("$t->blocked = false;")
    if op == "scatter":
        write_phase1_scatter_forall(builder, tr, inscription)
    elif op == "gather":
        write_phase1_gather_forall(builder, tr, inscription)
    elif op == "bcast":
        write_phase1_bcast_forall(builder, tr, inscription)
    elif op == "allgather":
        write_phase1_allgather_forall(builder, tr, inscription)

    builder.block_end()
    builder.block_end()
