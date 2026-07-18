---
title: "PTX opt layer: target-independent and native-first"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:04.496599+02:00"
---

Depth review: NO native optimizer exists (zero peephole/fold/CSE/DCE in src/habu; only bootstrap/cg/opt.fs, 240 lines gforth-side). When the PTX typed IR lands (docs/tma-gather.md campaign), build the opt layer target-independent + native-first or we get a third per-dialect optimizer. Also: do not copy the label-VARIABLE global-state emitter pattern (habu1/habu2/jit) into the PTX emitter — the emit.f line-sink shape is the better precedent.
