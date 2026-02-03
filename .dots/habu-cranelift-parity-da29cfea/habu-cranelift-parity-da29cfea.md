---
title: Cranelift parity
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T13:28:36.753427+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
---

Goal: close docs/cranelift-parity.md rows with executable proofs (tests+benches) and raise backend capabilities to Cranelift-class. Gate: fix GC/VM correctness first (habu-fix-repl-chunk-dd041c71, habu-fix-gc-work-76dc7cfe). Scope: SSA IR, lowering, ABI, regalloc, aarch64 backend, stack maps/safepoints, relocs, tiering, debug info, multi-ISA.
