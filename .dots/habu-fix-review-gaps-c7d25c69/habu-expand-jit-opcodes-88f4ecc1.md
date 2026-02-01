---
title: Expand JIT opcodes
status: open
priority: 3
issue-type: task
created-at: "2026-02-01T22:30:31.834238+01:00"
blocks:
  - habu-add-jit-globals-92322b58
---

Context: src/jit/jit.zig:318-340; cause: UnsupportedOpcode for globals/calls/closures; fix: implement globals/calls/closures/make_list/vec (see existing dots habu-add-jit-calls-fc73edd7, habu-implement-jit-globals-a2c6f875, habu-guard-jit-car-a4046df9); deps: habu-add-jit-globals-92322b58; verification: JIT parity tests
