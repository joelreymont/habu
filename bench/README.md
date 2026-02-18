# Habu Benchmarks

## Comprehensive Benchmark Suite

25 benchmarks comparing Habu vs SBCL across 10 categories:

| Category | Benchmarks |
|----------|-----------|
| arith | fixnum_loop, fixnum_mul, gcd |
| float | float_sum, float_sqrt |
| recurse | fib30, tak, ack, nqueens10 |
| list | list_build, list_reverse, list_append, assoc |
| hof | mapcar, reduce, remove_if |
| hash | hash_insert, hash_lookup |
| string | string_concat, string_search |
| sort | sort_fixnum, sort_string |
| gc | gc_cons, gc_vector |
| symbol | intern |

### Running

```bash
# Full comparison (Habu interpreter vs SBCL)
tools/comprehensive-bench

# With more iterations
tools/comprehensive-bench --iters=5

# JSON output
tools/comprehensive-bench --json

# Just Habu (via zig build, hoist-enabled JIT mode)
zig build -Duse-hoist=true bench-comp -- --iters=3

# Just SBCL
sbcl --script bench/comprehensive.lisp
```

### Files

- `bench/comprehensive_bench.zig` — Habu-side benchmark harness (Zig)
- `bench/comprehensive.lisp` — SBCL-side benchmark harness
- `bench/comprehensive.habu` — Habu benchmark expressions (for REPL testing)
- `tools/comprehensive-bench` — Combined runner script

### Notes

- Habu runs as a **bytecode interpreter**; SBCL always **compiles to native code**
- The ~400x gap is expected for interpreter vs native compiler
- JIT benchmarks (`tools/jit-bench`) show much closer results for JIT-compiled functions
- Benchmark sizes are tuned so Habu completes in ~20 seconds total

## JIT Microbenchmarks

```bash
# JIT benchmark (fixnum_loop, fib35, tak)
tools/jit-bench
```

## Other Benchmarks

```bash
zig build bench-cl                           # Original 5-item CL comparison
zig build -Duse-hoist=true bench-jit        # JIT microbenchmarks
zig build bench                             # GC benchmarks
zig build bench-vm                          # VM benchmarks
```
