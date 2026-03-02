---
title: Fix NLX control transfers
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.193948+02:00"
blocks:
  - habu-implement-restart-lookup-c7501041
---

Problem: catch/throw, block/return-from, and tagbody/go semantics are not fully aligned with real Maxima paths. Acceptance: non-local exits preserve dynamic state and unwind correctly. Files: src/interp/vm.zig NLX machinery. Verify: focused NLX regressions and Maxima load/runtime probes. Blockers: habu-implement-restart-lookup-c7501041.
