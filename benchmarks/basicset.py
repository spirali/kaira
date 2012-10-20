import benchmarks as b

# Manipulation with big tokens
b.add("bigarrays")
# Simple test
b.add("counter")
# Two independant counters
b.add("counter2", "counter2-t1").params(END=1500000)
# Two independant counters on two threads
b.add("counter2", "counter2-t2", threads=2).params(END=1500000)
# Benchmark of standard sample 'workers'
b.add("workers", processes=3).params(LIMIT=920000, SIZE=200)
# Simple net with huge number of small tokens
b.add("manytokens")
# 1000 starts of simple module called form C++ code
b.add_test("simplemodule", "run")

b.run(10)
