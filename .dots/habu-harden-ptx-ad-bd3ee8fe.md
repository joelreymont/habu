---
title: Harden PTX AD DAG validation
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:32:50.889285+02:00"
---

Deep-review finding 2026-06-27: lib/ptx/ad-dag.f uses fixed AD-MAXN arrays with unchecked AD-VPUSH/AD-VPOP/AD-VTOP/AD-NODE indexing, silently drops unknown opcodes, and accepts final symbolic stacks without requiring exactly one output. Correct fix: add named PTX AD errors for overflow, underflow, unknown opcode, and non-singleton final stack; bounds-check every array access; add regressions for >32 nodes, binary underflow, extra output, and invalid opcode.
