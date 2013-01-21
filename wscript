
from waflib import Logs

APPNAME = "Kaira"
VERSION = "0.5"

def options(ctx):
    ctx.add_option("--icc",
                   action="store_true",
                   default=False,
                   help="use ICC compiler instead of gcc")

    ctx.add_option("--disable-mpi",
                   action="store_true",
                   default=False,
                   help="do not build MPI")

    ctx.load("compiler_cxx")

def configure(ctx):
    if ctx.options.icc:
        ctx.load("icpc")
    else:
        ctx.load("g++")

    if not ctx.env.CXXFLAGS:
        ctx.env.append_value("CXXFLAGS", "-O2")
        ctx.env.append_value("CXXFLAGS", "-g")
    ctx.env.CAVERIF = \
        ctx.check_cxx(header_name="google/sparse_hash_set", mandatory=False)

    if not ctx.options.disable_mpi:
        if ctx.options.icc:
            ctx.find_program("mpiicc", var="MPICC", mandatory=False)
            ctx.find_program("mpiicpc", var="MPICXX", mandatory=False)
        else:
            ctx.find_program("mpicc", var="MPICC", mandatory=False)
            ctx.find_program("mpic++", var="MPICXX", mandatory=False)

        # -------------- MPI ENV ----------------
        ctx.setenv("mpi", ctx.env.derive())
        if ctx.env.MPICXX:
            ctx.env.CXX=ctx.env.MPICXX
            mpi=True
        elif ctx.env.MPICC:
            ctx.env.CXX=ctx.env.MPICC
            ctx.env.append_value("STLIB", "-lstdc++")
            mpi=True
        else:
            mpi=False
    else:
        mpi = False

    ctx.setenv("")
    ctx.env.HAVE_MPI = mpi

    if not ctx.env.CAVERIF:
        Logs.pprint("CYAN",
                    "Verification subsystem is disabled, because "
                    "the library google-sparsehash was not found.")

    if not mpi and not ctx.options.disable_mpi:
        Logs.pprint("CYAN",
                    "MPI support is disabled, because "
                    "no MPI implementation was found.")


def build(ctx):
    ctx.recurse("libs/cailie")
    ctx.recurse("libs/caserver")
    ctx.recurse("libs/caclient")
    ctx.recurse("libs/caverif")
