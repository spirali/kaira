
from utils import EqMixin
from neltypes import TypeVar, rename_vars, fresh_typevar, join_contexts_by_equations, join_contexts
from neltypes import t_string, t_int, t_array, t_tuple, t_bool
import neltypes
import utils

nel_standard_functions = {
#   name: [ return_type, args .. ]
    "+": [ t_int, t_int, t_int ],
    "*": [ t_int, t_int, t_int ],
    "-": [ t_int, t_int, t_int ],
    "/": [ t_int, t_int, t_int ],
    "<": [ t_bool, t_int, t_int ],
    ">": [ t_bool, t_int, t_int ],
    "==": [ t_bool, TypeVar(0), TypeVar(0) ],
    "!=": [ t_bool, TypeVar(0), TypeVar(0) ],
    "length": [ t_int, t_array(TypeVar(0)) ],
    "at": [ TypeVar(0), t_array(TypeVar(0)), t_int ],
}

class Env():

    def find_function_decl(self, name, args_count, safevars = True):
        ftypes = nel_standard_functions.get(name)
        if ftypes is None:
            raise Exception("Unknown function '{0}'".format(name))
        if len(ftypes) - 1 != args_count:
            raise Exception("Invalid number of arguments")
        if safevars:
            ftypes = rename_vars(ftypes)
        return (ftypes[0], ftypes[1:])


class Expression(object):

    nel_type = None

    def __init__(self):
        pass

    def get_direct_vars(self):
        return set()

    def get_undirect_vars(self):
        return self.get_free_vars().difference(self.get_direct_vars())

    def get_free_vars(self):
        return set()

    def get_direct_pairing(self, expr):
        return [ (expr, self)]


    def derive_context(self, t):
        """
        @param t NelType
        @return Context
        """
        return {}

    def __eq__(self, other):
        # Eq test is almost the same as EqMixin only we ignore variable nel_type
        # Eq test is called mostly in tests and we want to e1 == e2 even one of then has
        # not injected types
        return (isinstance(other, self.__class__)
            and utils.dict_eq(self.__dict__, other.__dict__, ("nel_type",)))

    def __ne__(self, other):
        return not self.__eq__(other)

class ExprLiteral(Expression):

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "{0}({1})".format(self.__class__.__name__, repr(self.value))

    def get_constraints(self, env):
        return (self.nel_type, {}, [])

    def inject_types(self, env, context):
        return # Literal has apriori nel_type

class ExprVar(Expression):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "ExprVar({0})".format(repr(self.name))

    def get_constraints(self, env):
        tv = fresh_typevar()
        return (tv, {self.name : tv} , [])

    def emit(self, emitter):
        return emitter.variable(self.name)

    def get_direct_vars(self):
        return set([self.name])

    def get_free_vars(self):
        return set([self.name])

    def inject_types(self, env, context):
        self.nel_type = context[self.name]

    def derive_context(self, t):
        return { self.name : t }


class ExprParam(Expression):

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "ExprParam({0})".format(repr(self.name))


class ExprInt(ExprLiteral):
    nel_type = t_int

    def emit(self, emitter):
        return emitter.const_int(self.value)

class ExprString(ExprLiteral):
    nel_type = t_string

    def emit(self, emitter):
        return emitter.const_string(self.value)

class ExprArray(Expression):

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "ExprArray({0.value})".format(self)

    def emit(self, emitter):
        return emitter.const_array(self.value, self.nel_type)

    def get_constraints(self, env):
        at = fresh_typevar()
        eqs = []
        ctx = {}
        for e in self.value:
            t, c, eq = e.get_constraints(env)
            eqs += eq
            ctx, q = join_contexts_by_equations(c, ctx)
            eqs += q
            eqs.append((t, at))
        return (t_array(at), ctx, eqs)


class ExprBool(ExprLiteral):
    nel_type = t_bool


nel_true = ExprBool(True)
nel_false = ExprBool(False)

class ExprCall(Expression):

    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __repr__(self):
        return "ExprCall({0},{1})".format(repr(self.name), repr(self.args))

    def get_constraints(self, env):
        returntype, targs = env.find_function_decl(self.name, len(self.args))
        ctx = {}
        eqs = []
        for targ, arg in zip(targs, self.args):
            t, c, eq = arg.get_constraints(env)
            eqs += eq
            eqs.append((t, targ))
            ctx, q = join_contexts_by_equations(c, ctx)
            eqs += q
        return (returntype, ctx, eqs)

    def emit(self, emitter):
        return emitter.call(self.name, self.args)

    def get_free_vars(self):
        return set().union(*[ e.get_free_vars() for e in self.args ])

    def inject_types(self, env, context):
        returntype, targs = env.find_function_decl(self.name, len(self.args))
        types = []
        for e in self.args:
            e.inject_types(env, context)
            types.append(e.nel_type)
        table = neltypes.unify(zip(types, targs))
        self.nel_type = returntype.replace_vars(table)

class ExprTuple(Expression):
    def __init__(self, args):
        self.args = args

    def __repr__(self):
        return "ExprTuple({0})".format(repr(self.args))

    def get_constraints(self, env):
        ts = []
        eqs = []
        ctx = {}
        for arg in self.args:
            t, c, eq = arg.get_constraints(env)
            eqs += eq
            ctx, q = join_contexts_by_equations(c, ctx)
            eqs += q
            ts.append(t)
        return (t_tuple(*ts), ctx, eqs)

    def emit(self, emitter):
        return emitter.tuple(self.nel_type, self.args)

    def get_direct_vars(self):
        return set().union(*[ e.get_direct_vars() for e in self.args ])

    def get_free_vars(self):
        return set().union(*[ e.get_free_vars() for e in self.args ])

    def get_direct_pairing(self, expr):
        result = []
        for i, e in enumerate(self.args):
            result += e.get_direct_pairing(ExprTupleGet(expr, i, len(self.args)))
        return result

    def inject_types(self, env, context):
        for e in self.args:
            e.inject_types(env, context)
        self.nel_type =  t_tuple( *[ e.nel_type for e in self.args ] )

    def derive_context(self, t):
        if t.name != "" and len(t.args) != len(self.args):
            raise Exception("Invalid type")
        context = {}
        for tt, e in zip(t.args, self.args):
            context = join_contexts(e.derive_context(tt), context)
        return context


class ExprExtern(Expression):

    def __init__(self, name, t):
        self.name = name
        self.nel_type = t

    def __repr__(self):
        return "ExprExtern({0}, {1})".format(repr(self.name), repr(self.nel_type))

    def inject_types(self, env, context):
        pass

    def emit(self, emitter):
        return emitter.extern(self.name)

class ExprCast(Expression):

    def __init__(self, expr, t):
        self.expr = expr
        self.type = t

    def get_direct_vars(self):
        return self.expr.get_direct_vars()

    def get_free_vars(self):
        return self.expr.get_free_vars()

    def get_constraints(self, env):
        t, c, eq = self.expr.get_constraints(env)
        eq.append((t, self.type))
        return (self.type, c, eq)


class ExprTupleGet(Expression):

    '''
        @param expr: Expression
        @param index: int
    '''
    def __init__(self, expr, index, tuplesize = None):
        if tuplesize is None:
            pass
        else:
            assert index >= 0 and index < tuplesize
        self.expr = expr
        self.tuplesize = tuplesize
        self.index = index

    def __repr__(self):
        return "ExprTupleGet({0}, {1}, {2})".format(repr(self.expr), self.tuplesize, self.index)

    def get_constraints(self, env):
        t, c, eq = self.expr.get_constraints(env)
        nt = [ fresh_typevar() for x in range(self.tuplesize) ]
        eq.append((t, t_tuple(*nt)))
        return (nt[self.index], c, eq)

    def inject_types(self, env, context):
        self.expr.inject_types(env, context)
        t = self.expr.nel_type
        assert t.name == "" # Type is tuple
        if self.tuplesize != None:
            assert self.tuplesize == len(t.args)
        else:
            self.tuplesize = len(t.args)
        self.nel_type = t.args[self.index]

    def emit(self, emitter):
        return emitter.tuple_get(self.expr, self.index)

class Instruction(EqMixin):

    def get_exprs(self):
        return ()

    def get_instructions(self):
        return ()

    def inject_types(self, env, context):
        for i in self.get_instructions():
            i.inject_types(env, context)
        for e in self.get_exprs():
            e.inject_types(env, context)

class ISet(Instruction):

    def __init__(self, varname, expr):
        self.varname = varname
        self.expr = expr

    def get_exprs(self):
        return (self.expr,)

    def __repr__(self):
        return "ISet({0}, {1})".format(repr(self.varname), repr(self.expr))

    def emit(self, emitter, writer):
        emitter.i_set(writer, self.varname, self.expr)

class INoop(Instruction):

    def __repr__(self):
        return "INoop"

    def emit(self, emitter, writer):
        pass

class IIf(Instruction):

    def __init__(self, expr, true, false = INoop()):
        self.expr = expr
        self.true = true
        self.false = false

    def __repr__(self):
        return "IIf({0}, {1}, {2})".format(repr(self.expr), repr(self.true), repr(self.false))

    def emit(self, emitter, writer):
        emitter.i_if(writer, self.expr, self.true, self.false)

class IExtern(Instruction):

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "IExtern({0})".format(self.name)

    def emit(self, emitter, writer):
        emitter.i_extern(writer, self.name)