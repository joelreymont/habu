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

| Area | Capability | Habu | Proof | Perf | ISA | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| Frontend | SSA IR | partial | `src/jit/ir.zig`: test "jit ir builds blocks and edges"; `src/jit/verify.zig`: test "jit ir verify ok"; `src/jit/print.zig`: test "jit ir dump smoke" | n/a | n/a | SSA IR exists (builder/verifier/printer); JIT codegen still lowers bytecode directly. |
| Codegen | Register allocation | no | n/a | n/a | n/a | Fixed register assignment today. |
| Codegen | Calling convention support | partial | n/a | n/a | aarch64 | C-ABI entry; helper calls use Zig error-union ABI; closures dispatch via rt.call/apply (no native ABI lowering). |
| Runtime | Stack maps / GC safepoints | no | n/a | n/a | n/a | GC via helpers with explicit root arrays; no stack maps or compiler-inserted safepoints. Design: `docs/stack-maps.md`. |
| Runtime | Relocations | no | n/a | n/a | n/a | Compile-time patching only; no relocation records or code movement. |
| Runtime | Deopt / OSR hooks | no | n/a | n/a | n/a | Typically handled by the embedding runtime. |
| Runtime | Tiering / profiling hooks | partial | `src/tests/jit_tiering.zig`: test "jit tiering hot threshold" | n/a | aarch64 | Hot-count threshold JIT tiering exists; no hot-loop detection/profiling yet. |
| Debug | DWARF / debug info | no | n/a | n/a | n/a | No debug info emitted. |
| ISA | Multi-ISA backend | no | n/a | n/a | n/a | AArch64 only. |
| Memory | W^X / icache management | yes | `src/jit/patch.zig`: test "code buffer" | n/a | aarch64 | macOS MAP_JIT + write-protect; non-mac uses mprotect; aarch64 icache flush. |

Last updated: 2026-02-03
