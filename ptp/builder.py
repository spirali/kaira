
from base.expressions import ISet, IIf, ExprExtern, ExprVar, IExtern, ExprCall
from base.utils import topological_ordering

def match_expression(env, context, expr, covered_vars, token):
        def depends_on(x, y):
            _, a = x
            _, b = y     
            return len(a.get_direct_vars().intersection(b.get_undirect_vars())) != 0
        
        pairing = expr.get_direct_pairing(token)
        
        for a, b in pairing:
            a.inject_types(env, context)
            b.inject_types(env, context)
        
        ordered = topological_ordering(pairing, depends_on)
        assert ordered is not None

        c = covered_vars.copy()
        code = []
        for e1, e2 in ordered:
            if isinstance(e2, ExprVar) and e2.name not in c:
                c.add(e2.name)
                code.append(ISet(e2.name, e1))
            else:
                code.append(IIf(ExprCall("!=", [ e2, e1 ]), IExtern("fail")),)
        return code, c
    
#    def inject_types(self, project):
#        for net in project.nets:
#            for tr in net.transitions:
#                ctx = tr.get_context()
#                tr.inject_types(ctx)

def get_ordered_types(project):
    types_set = project.get_all_types()
    return topological_ordering(list(types_set), lambda a, b: b.depends_on(a))

def get_edges_mathing(project, tr):
    env = project.get_env()
    context = tr.get_context()
    matches = []
    covered = set()
        
    for edge in tr.edges_in:
        token = ExprExtern("token", edge.get_place().type)
        instrs, covered = match_expression(env, context, edge.expr, covered, token)
        matches.append((edge, instrs))
    
    return matches