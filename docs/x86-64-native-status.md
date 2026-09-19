# x86-64 native port status

Baseline: the newest compiler lineage available in GitHub on 2026-09-19 is
`review/current-compiler` at `515463165054` (2026-09-13). No repository
commit from 2026-09-16..19 predates this experiment; the 2026-09-19 commits are
the typed-local and Intel-recovery work created in this session. This branch
combines those two current lineages rather than falling back to `master`
(August).

## Checkpoints

1. Intel-hosted ARM64 recovery compiler is reproducible and executes the real
   Habu checker/runtime under QEMU.
2. Latest typed-local work is integrated from `experiment/wide-typed-locals`.
3. Phase-A x86 instruction construction and bounded emission are committed as
   repository source, with the native-Habu relocation fixes.
4. The target contract now has append-only `x86-64` and
   `sysv-amd64-linux` variants. Existing wire codes and schema version are
   intentionally unchanged.
5. Native x86 HIR selection/allocation/runtime/selfbuild are not yet complete.

## Architectural blocker now being worked

The current native chain is not a generic backend with an A64 implementation.
Its machine IR, effect model, selector, register allocator, spill pass, emitter,
publisher, runtime register reservation, JIT layout, and compiler driver are all
A64-specific. A correct Intel port therefore cannot be completed by swapping the
encoder under `A64EMIT`: the chain has to gain an x86 machine dialect and
backend boundary through selection, allocation, publication, runtime and image
construction.

A port is only called finished when a native x86 ELF (not QEMU ARM64) builds,
runs the checker, passes the native/compiler/runtime gates, rebuilds itself with
JIT entry disabled during selfbuild, and reaches the accepted deterministic
identity/provenance gates.
