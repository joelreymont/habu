---
title: Cranelift parity
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T13:28:36.753427+01:00"
blocks:
  - habu-perf-design-spec-b61a2f8f
  - habu-update-perf-doc-de5e450d
  - habu-check-jit-compile-f045c68c
  - habu-add-jit-code-7ae9d76b
  - habu-check-jit-code-7c2c88d1
  - habu-update-code-size-6f736178
  - habu-test-jit-code-4bd08f3d
  - habu-print-jit-code-cc258b3f
  - habu-regalloc-design-206abbeb
  - habu-reloc-design-074ee347
  - habu-stack-maps-design-b430999e
  - habu-parity-matrix-update-da14d25a
  - habu-parity-tests-9be195f5
  - habu-jit-ir-3ffafaa9
  - habu-bench-harness-f817afd6
  - habu-add-jit-cons-bd49c0de
  - habu-update-parity-matrix-00bb1636
  - habu-test-jit-tiering-198b18e1
  - habu-update-tiering-proof-c3601f32
  - habu-add-jit-num-7dffed90
  - habu-add-jit-mod-cfdd286a
  - habu-add-rootset-types-cf9db4d2
  - habu-add-rootset-gc-be933d07
  - habu-port-heap-gc-b1aecb37
  - habu-add-heap-rootset-7bdae262
  - habu-port-vm-gc-8379cde6
  - habu-fix-repl-chunk-827c53bb
  - habu-fix-macro-chunk-b024626e
  - habu-fix-compiler-env-b6578045
  - habu-fix-gc-slot-1e259046
  - habu-ir-skeleton-d7b83105
---

Goal: close docs/cranelift-parity.md rows with executable proofs (tests+benches) and raise backend capabilities to Cranelift-class. Gate: fix GC/VM correctness first (habu-fix-repl-chunk-827c53bb, habu-fix-gc-work-76dc7cfe). Scope: SSA IR, lowering, ABI, regalloc, aarch64 backend, stack maps/safepoints, relocs, tiering, debug info, multi-ISA.
