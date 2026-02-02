---
title: Update parity doc
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-02T22:28:22.188194+01:00\\\"\""
closed-at: "2026-02-02T22:46:39.399316+01:00"
close-reason: Audit JIT parity notes
blocks:
  - habu-validate-imm32-patch-ee04f8bb
---

Context: docs/cranelift-parity.md:1; cause: parity matrix needs audit vs current JIT + VM; fix: verify each row vs code (jit.zig/rt.zig/vm.zig) and update notes; deps: habu-validate-imm32-patch-ee04f8bb; verification: doc updated
