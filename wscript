
from waflib import Logs, Context

APPNAME = "Kaira"
VERSION = "1.3"

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

    ctx.add_option("--disable-clang",
                   action="store_true",
                   default=False,
                   help="do not build clang support")

    ctx.add_option("--disable-doc",
                   action="store_true",
                   default=False,
                   help="do not build HTML documentation")

    ctx.add_option("--compiler-prefix",
                    action="store",
                    default="")

    ctx.add_option("--clang-path",
                    action="store",
                    default=None)

    ctx.load("compiler_cxx")
    ctx.load("python")


def configure(ctx):
    if ctx.options.icc:
        ctx.load("icpc")
    else:
        ctx.load("g++")
    ctx.load("python")
    ctx.check_python_version((2,7,0))

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
            ctx.env.CXX = list(ctx.env.MPICXX)
            mpi=True
        elif ctx.env.MPICC:
            ctx.env.CXX = list(ctx.env.MPICC)
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

    if not ctx.options.disable_clang:
        if ctx.options.clang_path:
            libclang = ctx.root.find_node(ctx.options.clang_path)
            if libclang is None:
                 ctx.fatal("%s not found" % ctx.options.clang_path)
            libclang = ctx.options.clang_path
        else:
            tmp = ctx.root.ant_glob("usr/lib/**/libclang.so*", dir=True, maxdepth=4, excl=["**/ssl*"])
            if not tmp:
                    ctx.fatal("libclang.so not found.\n"
                              "Use --disable-clang to disable code complete functions, or "
                              "--clang-path=PATH to specify the path.")
            libclang = tmp[0].abspath()
        ctx.msg("libclang", libclang)
    else:
        libclang = None
    ctx.env.LIBCLANG = libclang


def _create_files(ctx):
    # --------- run_python -------------
    run_python = ctx.bldnode.make_node("run_python")
    run_python.write("#!/bin/sh\n")
    run_python.write("exec " + ctx.env.PYTHON[0] + " $@\n")

    # --------- config.ini -----------
    conffile = ctx.bldnode.make_node("config.ini")
    conffile.parent.mkdir()
    myconf = []
    myconf.append("[Main]")
    myconf.append("VERSION: " + VERSION)
    myconf.append("CXX: " + " ".join(ctx.env.CXX))
    myconf.append("OCTAVE: " + str(bool(ctx.env.MKOCTFILE)))
    myconf.append("PYTHON: " + ctx.env.PYTHON[0])
    myconf.append("LIBCLANG: " + str(bool(ctx.env.LIBCLANG)))

    if ctx.env.HAVE_MPI:
        myconf.append("MPICXX: " + " ".join(ctx.all_envs["mpi"].CXX))
    if ctx.env.MKOCTFILE:
        myconf.append("[Octave]")
        incflags = ctx.cmd_and_log([ctx.env.MKOCTFILE[0], "--print", "INCFLAGS" ],
                                   output=Context.STDOUT, quiet=Context.BOTH)
        lflags = ctx.cmd_and_log([ctx.env.MKOCTFILE[0], "--print", "LFLAGS" ],
                                   output=Context.STDOUT, quiet=Context.BOTH)
        libs = ctx.cmd_and_log([ctx.env.MKOCTFILE[0], "--print", "OCTAVE_LIBS" ],
                                   output=Context.STDOUT, quiet=Context.BOTH)
        myconf.append("INCFLAGS: " + incflags)
        myconf.append("LFLAGS: " + lflags)
        myconf.append("LIBS: " + libs)

    if ctx.env.LIBCLANG:
        myconf.append("[libclang]")
        myconf.append("PATH: " + ctx.env.LIBCLANG)

    conffile.write("\n".join(myconf))
    ctx.env.append_value('cfg_files', conffile.abspath())

def build(ctx):
    _create_files(ctx)
    ctx.recurse("libs/cailie")
    ctx.recurse("libs/caserver")
    ctx.recurse("libs/caclient")
    ctx.recurse("libs/caverif")
    ctx.recurse("libs/casimrun")
    ctx.recurse("docs")
