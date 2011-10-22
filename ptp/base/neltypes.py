
from base.utils import EqMixin

class Type(EqMixin):

    def __init__(self, name, args = []):
        self.name = name
        self.args = tuple(args)

    def __repr__(self):
        if self.args:
            return "{0}({1})".format(self.name, ",".join(map(repr, self.args)))
        else:
            return self.name

    def get_variables(self):
        return set().union(*[ t.get_variables() for t in self.args ])

    def replace_vars(self, table):
        return Type(self.name, [ t.replace_vars(table) for t in self.args ])

    def get_arity(self):
        return len(self.args)

    def get_safe_name(self):
        if len(self.args) == 0:
            return self.name
        else:
            name = self.name if self.name != "" else "Tuple"
            return "{0}{1}_{2}".format(name, len(self.args), "_".join( [ t.get_safe_name() for t in self.args ]))

    def get_subtypes(self):
        result = set([self])
        for t in self.args:
            result.update(t.get_subtypes())
        return result

    def depends_on(self, t):
        return t != self and t in self.get_subtypes()

    def __hash__(self):
        return hash(self.name)

class TypeVar(EqMixin):

    def __init__(self, id):
        self.id = id

    def __repr__(self):
        return "TypeVar({0})".format(self.id)

    def get_variables(self):
        return set([self.id])

    def replace_vars(self, table):
        v = table.get(self.id)
        if v:
            return v
        else:
            return self

t_int = Type("Int")
t_string = Type("String")
t_bool = Type("Bool")
def t_array(x):
    return Type("Array", [x])
def t_tuple(*args):
    return Type("", args)

typevar_counter = 10
def fresh_typevar():
    global typevar_counter
    typevar_counter += 1
    return TypeVar(typevar_counter)

def join_contexts(ctx1, ctx2):
    context = ctx1.copy()
    for k, v in ctx2.items():
        tp = context.get(k)
        if tp is None:
            context[k] = v
        elif tp != v:
            raise Exception("Type mismatch {0}/{1}".format(tp, v))
    return context

def join_contexts_by_equations(ctx1, ctx2):
    equations = []
    ctx = ctx2.copy()
    for (key, v1) in ctx1.items():
        v2 = ctx2.get(key)
        if v2:
            equations.append((v1, v2))
        else:
            ctx[key] = v1
    return ctx, equations

def rename_vars(types):
    vars = set().union(*[ t.get_variables() for t in types ])
    table = {}
    for v in vars:
        table[v] = fresh_typevar()
    return [ t.replace_vars(table) for t in types ]

def replace_equations(table, equations):
    result = []
    for (t1, t2) in equations:
        result.append((t1.replace_vars(table), t2.replace_vars(table)))
    return result

def derive_context(env, defs):
    context = {}
    equations = []
    for e, t in defs:
        tp, c, eq = e.get_constraints(env)
        equations += eq
        equations.append((t, tp))
        context, eq = join_contexts_by_equations(context, c)
        equations += eq
    table = unify(equations)
    result = {}
    for key in context:
        result[key] = context[key].replace_vars(table)
    return result

def unify(equations):
    if len(equations) == 0:
        return {}
    t1, t2 = equations[-1]
    equations.pop()

    if t1 == t2:
        return unify(equations)
    if isinstance(t1, TypeVar):
        result = unify(replace_equations( { t1.id: t2 }, equations))
        result[t1.id] = t2.replace_vars(result)
        return result
    if isinstance(t2, TypeVar):
        result = unify(replace_equations( { t2.id: t1 }, equations))
        result[t2.id] = t1.replace_vars(result)
        return result
    if t1.name == t2.name and len(t1.args) == len(t1.args):
        return unify(equations + zip(t1.args, t2.args))
    raise Exception("Type mismatch: {0}/{1}".format(t1, t2))