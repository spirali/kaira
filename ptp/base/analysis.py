
import utils

def all_free_variables(edges):
    return utils.unions(edges, lambda edge: edge.get_free_vars())

def analyze_transition(tr):

    # Get rid of packing edges or with taget or with guard
    edges_out = [ edge for edge in tr.edges_out
                    if edge.is_normal() and edge.is_local() and edge.guard is None ]

    edges_in = [ edge for edge in tr.edges_in
                    if edge.is_normal() ]

    var_edge = {}

    if tr.subnet is not None:
        # Skip optimizations for transitions with submodule
        for var in all_free_variables(edges_in):
            for edge in edges_in:
                if var in edge.expr.get_direct_vars():
                    var_edge[var] = edge
                    break
        tr.var_edge = var_edge
        return


    bounded = set()
    var_edge = {}

    for edge in edges_in:
        if not edge.expr.is_direct_expression():
            continue

        edge_out = utils.find_first(edges_out, lambda e: e.expr == edge.expr)
        if edge_out is None:
            continue

        v = edge.expr.get_free_vars()
        if v.intersection(bounded): # Any variable cannot be bounded yet
            continue
        bounded.update(v)

        for var in v:
            var_edge[var] = edge

        edge_out.token_source = edge
        edge.token_reused = True

    for var in all_free_variables(edges_in).difference(bounded):
        for edge in edges_in:
            if var in edge.expr.get_direct_vars():
                var_edge[var] = edge
                break

    tr.var_edge = var_edge
