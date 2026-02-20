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
- `bench/maxima_workload.zig` — Habu real-workload Maxima harness
- `bench/maxima_workload.lisp` — SBCL real-workload Maxima harness
- `bench/sbcl_gc.lisp` — SBCL GC stress benchmark
- `tools/comprehensive-bench` — Combined runner script
- `tools/maxima-bench` — Maxima workload comparison runner
- `tools/gc-compare` — Habu vs SBCL GC comparison runner
- `tools/perf-loop` — Self-improvement loop (ranked bottlenecks)
  - Includes `gc_compare` JSON block with gate schema/metrics from `tools/gc-compare`

### GC Parity Gates

`tools/gc-compare` now emits numeric GC gate targets for both `vs_sbcl` and `vs_ocaml`.
Current execution path evaluates `vs_sbcl` and reports per-level pass/fail.
`bench/gc.zig` JSON now includes pause percentiles (`p50_pause_ns`, `p95_pause_ns`, `p99_pause_ns`), phase-mode timings (`gc_minor_count`, `gc_major_count`, `avg_minor_ns`, `avg_major_ns`), allocation sampling telemetry (`alloc_sample_*`, `alloc_sample_size`), survival/promotion histograms (`gc_survive_*`, `gc_promote_*`), and adaptive nursery policy telemetry (`gc_nursery_*`).
`bench/check.zig` enforces `gc_nursery_target >= live_bytes` so adaptive shrinking cannot set a trigger below live nursery occupancy.
`tools/gc-compare --with-maxima` augments micro-GC gates with Maxima workload GC telemetry (default stress point: `--maxima-scale=3 --maxima-nursery-mb=24`).
CI mode:
- `--gate-level=<milestone_2x_from_baseline|competitive|parity>`
- `--fail-on-gates` (exit `1` when selected gate fails)

Levels and thresholds:

| Level | avg_pause_ratio_min (`sbcl/habu`) | p95_pause_ratio_min (`sbcl/habu`) | throughput_ratio_min (`habu/sbcl`) | rss_ratio_max (`habu/sbcl`) |
|----------|-----------|-----------|-----------|-----------|
| milestone_2x_from_baseline | 0.1340 | 0.2224 | 0.1340 | 4.0 |
| competitive | 0.50 | 0.50 | 0.50 | 2.0 |
| parity | 1.00 | 1.00 | 1.00 | 1.20 |

`baseline_date=2026-02-20` uses measured `vs_sbcl` baseline ratios:
- `avg_pause_ratio=0.0670`
- `p95_pause_ratio=0.1112`
- `throughput_ratio=0.0670`

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
zig build -Duse-hoist=true bench-maxima     # Maxima real-workload benchmark (generational)
tools/maxima-bench                          # Habu vs SBCL Maxima workload
tools/maxima-bench --scale=3 --nursery-mb=24
tools/gc-compare --iters=30 --live-mb=8     # Habu vs SBCL GC pause comparison
tools/gc-compare --with-maxima              # Include Maxima GC telemetry in comparison
tools/gc-compare --json                      # Include gate metrics + threshold status
tools/gc-compare --fail-on-gates --gate-level=milestone_2x_from_baseline
tools/perf-loop --iters=1 --scale=1         # Ranked bottlenecks + GC gate summary
tools/perf-loop --json --gc-iters=30        # Include gc_compare gate schema in JSON
tools/perf-loop --fail-on-gates --gate-level=milestone_2x_from_baseline
```
