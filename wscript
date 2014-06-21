
from waflib import Logs, Context

APPNAME = "Kaira"
VERSION = "1.2"

def options(ctx):
    ctx.add_option("--icc",
                   action="store_true",
                   default=False,
                   help="use ICC compiler instead of gcc")

    ctx.add_option("--disable-mpi",
                   action="store_true",
                   default=False,
                   help="do not build MPI")

    ctx.add_option("--disable-octave",
                   action="store_true",
                   default=False,
                   help="do not build Octave subsystem")

    ctx.add_option("--disable-verif",
                   action="store_true",
                   default=False,
                   help="do not build verification subsystem")

    ctx.add_option("--disable-doc",
                   action="store_true",
                   default=False,
                   help="do not build HTML documentation")

    ctx.add_option("--compiler-prefix",
                    action="store",
                    default="")

    ctx.load("compiler_cxx")


def configure(ctx):
    if ctx.options.icc:
        ctx.load("icpc")
    else:
        ctx.load("g++")

    compiler_prefix = ctx.options.compiler_prefix.split()
    if compiler_prefix:
        ctx.env.prepend_value("CXX", compiler_prefix)

    if not ctx.env.CXXFLAGS:
        ctx.env.append_value("CXXFLAGS", "-O2")
        ctx.env.append_value("CXXFLAGS", "-g")
        ctx.env.append_value("CXXFLAGS", "-fPIC")

    ctx.env.append_value("CXXFLAGS", "-Wall")

    if not ctx.options.disable_verif:
        verif =  ctx.check_cxx(header_name="google/sparse_hash_map",
                               mandatory=False)
        verif = verif and ctx.check_cxx(header_name="mhash.h",
                                        lib="mhash",
                                        mandatory=False)
        if not verif:
            ctx.fatal("Dependacy for verification subsystem is missing.\n"
                      "For build without this subsystem use --disable-verif")
        ctx.env.HAVE_VERIF = True
    else:
        ctx.env.HAVE_VERIF = False

    if not ctx.options.disable_octave:
        ctx.find_program("mkoctfile", var="MKOCTFILE")

    if not ctx.options.disable_doc:
        ctx.find_program("asciidoc", var="ASCIIDOC")
    ctx.env.HAVE_DOC = not ctx.options.disable_doc

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
            ctx.env.CXX = [ ctx.env.MPICXX ]
            mpi=True
        elif ctx.env.MPICC:
            ctx.env.CXX = [ ctx.env.MPICC ]
            ctx.env.append_value("STLIB", "-lstdc++")
            mpi=True
        else:
            mpi=False
        if compiler_prefix:
            ctx.env.prepend_value("CXX", compiler_prefix)
    else:
        mpi = False

    ctx.setenv("")
    ctx.env.HAVE_MPI = mpi

    if not mpi and not ctx.options.disable_mpi:
        Logs.pprint("CYAN",
                    "MPI support is disabled, because "
                    "no MPI implementation was found.")

    conffile = ctx.bldnode.make_node("config.ini")
    conffile.parent.mkdir()
    myconf = []
    myconf.append("[Main]")
    myconf.append("VERSION: " + VERSION)
    myconf.append("CXX: " + " ".join(ctx.env.CXX))
    myconf.append("OCTAVE: " + str(bool(ctx.env.MKOCTFILE)))
    if mpi:
        myconf.append("MPICXX: " + " ".join(ctx.all_envs["mpi"].CXX))
    if ctx.env.MKOCTFILE:
        myconf.append("[Octave]")
        incflags = ctx.cmd_and_log([ctx.env.MKOCTFILE, "--print", "INCFLAGS" ],
                                   output=Context.STDOUT)
        lflags = ctx.cmd_and_log([ctx.env.MKOCTFILE, "--print", "LFLAGS" ],
                                   output=Context.STDOUT)
        libs = ctx.cmd_and_log([ctx.env.MKOCTFILE, "--print", "OCTAVE_LIBS" ],
                                   output=Context.STDOUT)
        myconf.append("INCFLAGS: " + incflags)
        myconf.append("LFLAGS: " + lflags)
        myconf.append("LIBS: " + libs)
    conffile.write("\n".join(myconf))
    ctx.env.append_value('cfg_files', conffile.abspath())

def build(ctx):
    ctx.recurse("libs/cailie")
    ctx.recurse("libs/caserver")
    ctx.recurse("libs/caclient")
    ctx.recurse("libs/caverif")
    ctx.recurse("libs/casimrun")
    ctx.recurse("docs")
