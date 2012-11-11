import benchmarks as b

b.set_comment("kaira")

# Manipulation with big tokens
b.project("bigarrays")
# Simple test
b.project("counter")
# Two independant counters
b.project("counter2", "counter2-t1").params(END=1500000)
# Two independant counters on two threads
b.project("counter2", "counter2-t2", threads=2).params(END=1500000)
# Benchmark of standard sample 'workers'
b.project("workers", processes=3).params(LIMIT=920000, SIZE=200)
# Simple net with huge number of small tokens
b.project("manytokens")
# 1000 starts of simple module called form C++ code
b.project_test("simplemodule", "run")

b.run(10)
