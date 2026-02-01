# Cranelift Parity Matrix

Legend: yes / partial / no. "Cranelift" refers to typical codegen capabilities; embedding runtimes may add or omit features.

| Area | Capability | Habu JIT | Cranelift | Notes |
| --- | --- | --- | --- | --- |
| Frontend | SSA IR | no | yes | Habu JIT lowers bytecode directly. |
| Codegen | Register allocation | no | yes | Habu uses fixed regs. |
| Codegen | Calling convention support | partial | yes | Habu calls not implemented yet. |
| Runtime | Stack maps / GC safepoints | no | yes | Needed for precise GC with native frames. |
| Runtime | Relocations | no | yes | Habu patches immediates manually. |
| Runtime | Deopt / OSR hooks | no | partial | Typically handled by the embedding runtime. |
| Runtime | Tiering / profiling hooks | no | partial | Habu has no hot-loop detection yet. |
| Debug | DWARF / debug info | no | yes | Habu emits no debug info. |
| ISA | Multi-ISA backend | no | yes | Habu targets ARM64 only. |
| Memory | W^X / icache management | yes | yes | Habu uses mmap + icache flush. |
