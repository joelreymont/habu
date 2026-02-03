# Cranelift Parity Performance Targets

This document defines baseline performance targets and measurement rules for
the cranelift-parity matrix. Targets are intentionally conservative until
benchmarks land; update numbers only with measurements.

## Bench principles

- **Measure, don't assume.** Every target must have a benchmark and a command.
- **Stable harness.** Use `zig build bench` or a dedicated runner so results are
  reproducible.
- **Report variance.** Provide median + p90, with N>=20 runs when possible.
- **Pin environment.** Record CPU model, OS version, Zig version.

## Target categories

### JIT compile latency

Goal: keep cold compile time low enough for REPL and small scripts.

- Metric: wall-clock compile time per function.
- Target: <= 200us median for a 50-op function; <= 2ms for 500-op.
- Proof: `bench/jit.zig` via `zig build bench-jit -- --json` (compile_ns/compile_n).

### Warm execution throughput

Goal: JIT steady-state should beat the interpreter for hot loops.

- Metric: ops/sec on tight arithmetic and list loops.
- Target: >= 5x interpreter on AArch64 for loop microbench.
- Proof: `bench/jit.zig` via `zig build bench-jit -- --json` (steady ops/sec).

### Allocation + GC pause

Goal: GC pauses bounded and amortized; no pathological spikes.

- Metric: max pause (ms) and total GC time over N allocations.
- Target: max pause <= 5ms for 1e6 cons allocations in 64MB heap.
- Proof: `bench/gc.zig` via `zig build bench -- --json` (p95_pause_ns).

### Code size / memory

Goal: native code size bounded and W^X overhead stable.

- Metric: bytes of code emitted per bytecode op.
- Target: <= 16 bytes/op median.
- Proof: `bench/jit.zig` via `zig build bench-jit -- --json` (code_bytes; per-op pending).

## Updating parity matrix

When a benchmark lands, add its path/command to the **Perf** column for the
relevant row in `docs/cranelift-parity.md`. Include the target and hardware in
Notes if the row is marked `yes`.
