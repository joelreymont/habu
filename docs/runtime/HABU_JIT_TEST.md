# HABU_JIT_TEST quick guide

- Purpose: opt-in ARM64 JIT smoke using `jit-eval` and real runtime addresses.
- Prereqs: helper `bin/print-runtime-addrs` built (`make runtime-addrs`), ARM64 host, `HABU_JIT_TEST=1`.
- Run: `HABU_JIT_TEST=1 sbcl --noinform --non-interactive --load tests/jit-cons-car-cdr.lisp`
- Behavior: loads `run-habu.lisp`, ensures runtime addrs (env/helper), runs `jit-eval` for cons/car/cdr; checks car/cdr == 1/2 (untagged).
- Notes: JIT remains opt-in; fails if runtime addresses wrong or codegen returns unexpected tagging. Adjust as we refine tagging. 
