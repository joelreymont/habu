---
title: Add integerp/realp
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-05T12:11:38.644118+01:00\\\"\""
closed-at: "2026-02-05T13:11:42.842938+01:00"
close-reason: Add integerp and realp
---

docs/cl-symbols.md:1099,1112: mark integerp/realp implemented (and refresh header counts).
src/compiler/ir.zig:572: add IR nodes + IrBuilder methods.
src/compiler/compile.zig:242,805: intern + dispatch as unary primitives.
src/bytecode/opcodes.zig:275: add bytecode ops; src/bytecode/emit.zig: wire emission.
src/interp/vm.zig:1847: implement VM semantics.
src/jit/rt.zig:125 and src/jit/jit.zig:239: add JIT fast-paths.
src/tests/jit_parity.zig:218: add VM/JIT parity cases.

Root cause: integerp/realp missing from the compiler+VM opcode set.
Fix: implement as IR+bytecode unary predicates (like numberp), with matching VM+JIT implementations.
Proof: `zig build test`; `python3 tools/cl_symbols_audit.py` reports ✗ 11.
