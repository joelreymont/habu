# Cranelift Parity Matrix

Legend: yes / partial / no. "Cranelift" refers to typical codegen capabilities; embedding runtimes may add or omit features.

| Area | Capability | Habu JIT | Cranelift | Notes |
| --- | --- | --- | --- | --- |
| Frontend | SSA IR | no | yes | Habu JIT lowers bytecode directly. |
| Codegen | Register allocation | no | yes | Habu uses fixed regs. |
| Codegen | Calling convention support | partial | yes | C-ABI entry; helper calls use Zig error-union ABI; closures dispatch via rt.call/apply (no native ABI lowering). |
| Runtime | Stack maps / GC safepoints | no | yes | GC via helpers with explicit root arrays; no stack maps or compiler-inserted safepoints. |
| Runtime | Relocations | no | yes | Compile-time patching only; no relocation records or code movement. |
| Runtime | Deopt / OSR hooks | no | partial | Typically handled by the embedding runtime. |
| Runtime | Tiering / profiling hooks | no | partial | Habu has no hot-loop detection yet. |
| Debug | DWARF / debug info | no | yes | Habu emits no debug info. |
| ISA | Multi-ISA backend | no | yes | Habu targets ARM64 only. |
| Memory | W^X / icache management | yes | yes | macOS MAP_JIT + write-protect; non-mac uses mprotect; aarch64 icache flush. |

Last audited: 2026-02-02 (src/jit/jit.zig, src/jit/patch.zig, src/jit/rt.zig, src/interp/vm.zig).
