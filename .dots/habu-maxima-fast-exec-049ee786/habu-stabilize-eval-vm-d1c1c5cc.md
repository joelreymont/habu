---
title: Stabilize eval VM paths
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:04.173423+01:00"
blocks:
  - habu-close-cl-semantic-dac2c058
---

src/interp/vm.zig and src/interp/repl.zig. Cause: macro-heavy loads hit non-local-exit and call-path fragility. Fix: root-cause and remove indirect-call/JIT path failures and eval exit bugs.
