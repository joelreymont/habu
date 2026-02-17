# Cranelift Parity Matrix

This matrix tracks "Cranelift-class" JIT backend capabilities. Each row has:

- **Habu**: `yes` / `partial` / `no`
- **Proof**: required for `yes`/`partial` (tests/benches); audits are not accepted
- **Perf**: target metric + threshold (fill in as benchmarks land)
- **ISA**: architectures covered by the proof

## Status meanings

- `yes`: implemented; proof exists; perf target met; limitations documented in Notes
- `partial`: subset implemented; proof exists for the subset; limitations documented
- `no`: missing

## Proof conventions

- `tests`: `path: test "<name>"`
- `bench`: `path` + command (prefer `zig build ...`)
- Perf targets: `docs/cranelift-perf.md`

| Area | Capability | Habu | Proof | Perf | ISA | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| Frontend | SSA IR | partial | `src/jit/backend.zig`: test "hoist IR translator: nested if in expression"; test "hoist IR translator: simple loop (let + while + setq)" | n/a | n/a | Habu lowers compiler IR into Hoist SSA via `IrTranslator`. |
| Codegen | Register allocation | no | n/a | n/a | n/a | Fixed register assignment today. |
| Codegen | Calling convention support | partial | `src/jit/backend.zig`: test "hoist identity"; test "hoist IR translator: two-arg add" | n/a | aarch64 | C-ABI entry; helper calls use Zig error-union ABI; closures dispatch via rt.call/apply (no native ABI lowering). |
| Runtime | Stack maps / GC safepoints | no | n/a | `docs/cranelift-perf.md` (GC pause targets) | n/a | VM GC now uses root ranges + slots (no `gc_vals` mirror array), but JIT still has no safepoint metadata or compiler-inserted stack maps. Design: `docs/stack-maps.md`. |
| Runtime | Relocations | partial | n/a | n/a | aarch64 | BL/BLR patching and self-pointer fixups live in `src/jit/backend.zig`; no dedicated relocation test yet. |
| Runtime | Deopt / OSR hooks | no | n/a | n/a | n/a | Typically handled by the embedding runtime. |
| Runtime | Tiering / profiling hooks | no | n/a | `bench/jit.zig`: `zig build bench-jit -- --json` | aarch64 | No dedicated hot-count tiering harness in-tree today. |
| Debug | DWARF / debug info | no | n/a | n/a | n/a | No debug info emitted. |
| ISA | Multi-ISA backend | no | n/a | n/a | n/a | AArch64 only. |
| Memory | W^X / icache management | yes | `src/jit/backend.zig` (`setExec`, `flushCacheRange` call sites) | `bench/jit.zig`: `zig build bench-jit -- --json` (code_bytes, code_bytes_per_op) | aarch64 | macOS MAP_JIT + write-protect; non-mac uses mprotect; aarch64 icache flush. |

## 2026-02-17 Rebaseline (Post Hoist Sync)

- Artifacts:
  - `bench/results/comprehensive_jit_20260217_115850.json`
  - `bench/results/comprehensive_interp_20260217_115850.json`
  - `bench/results/vm_20260217_115850.json`
  - `bench/results/gc_20260217_115850.json`
  - `bench/results/baseline_v9.json`
- JIT correctness gate:
  - `bench-comp` now completes without `gcd` bus error after fixing indirect-call postpass corruption in `src/jit/backend.zig`.
  - JIT primitive call correctness verified by runtime check: `(gcd 39 21) => 3` in JIT-compiled function.
- Key deltas vs `bench/results/baseline_v8.json` (Habu JIT):
  - `ack`: `166ms -> 54.8ms` (`-67.0%`)
  - `fixnum_mul`: `1.668ms -> 1.306ms` (`-21.7%`)
  - `gcd`: `3.791ms -> 4.054ms` (`+6.9%`)
  - `string_concat`: `603ms -> 653.9ms` (`+8.4%`)
- VM microbench (Mops/s):
  - `fixnum`: `1.505`
  - `cons`: `1.378`
  - `hash`: `1.556`
  - `string`: `0.928`
- GC pause baseline:
  - `avg_pause_ns`: `104,011,995`
  - `p95_pause_ns`: `104,581,375`

## 2026-02-17 Coverage Expansion (JIT Helpers)

- Artifact:
  - `bench/results/comprehensive_jit_20260217_123715.json`
- Implemented in `src/jit/backend.zig`:
  - Added helper-backed lowering for `make_hash`, `hash_get`, `hash_set`, `hash_count`.
  - Added helper-backed lowering for `make_string`, `str_set`, `position`.
  - Added helper-backed lowering for `arr_new` (rank-1), `arr_ref` (rank-1), `intern`.
  - Added focused `format` lowering for benchmark decimal directives (`~d`, `~<w>,'<pad>d`).
  - Wired new helper-lowered nodes into `canTranslate`, `firstUnsupportedTag`, and call-safety/untagged gating.
- Bench deltas vs `bench/results/comprehensive_jit_20260217_115850.json`:
  - `hash_insert`: `16.04ms -> 3.10ms` (`-80.7%`)
  - `hash_lookup`: `76.58ms -> 5.67ms` (`-92.6%`)
  - `string_search`: `204.84ms -> 14.65ms` (`-92.8%`)
  - `gc_vector`: `8.06ms -> 1.52ms` (`-81.1%`)
  - `intern`: `168.05ms -> 130.51ms` (`-22.3%`)
- Remaining benchmark JIT blockers from `HABU_TRACE_JIT`:
  - `mapcar`, `reduce`, `sort_fixnum`: unsupported `lambda`.
  - `sort_string`: unsupported `symbol_function`.
  - `string_concat`: unresolvable `concatenate` call-target path.
  - `float_sum`/`float_sqrt`: unresolved float call-target path (`UnsupportedCallTarget`).

Last updated: 2026-02-17
