---
title: Make JIT accounting truthful
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.363941+02:00"
blocks:
  - habu-make-benchmark-bring-617f62fe
---

Problem: helper and runtime JIT candidate matching and status reporting can diverge or silently skip. Acceptance: candidate identity, compile status, skip reasons, and unsupported-node coverage are canonical and auditable. Files: src/jit/candidates.zig:28-31,106-159,289-307,610-630; src/testing/compile_chunk.zig:344-408,489-509; src/interp/repl.zig:3066-3135,3238-3274. Verify: compile helper and runtime admit or reject the same candidates with explicit counters. Blockers: habu-make-benchmark-bring-617f62fe.
