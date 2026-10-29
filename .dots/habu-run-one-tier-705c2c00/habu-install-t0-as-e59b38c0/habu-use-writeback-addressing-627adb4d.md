---
title: Use writeback addressing for T0 one-cell spill and fill
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T04:43:33.353917+02:00"
---

Workspace: jerry-t0c on nspsunws. A5 profile: T0 moves the data stack pointer with a separate add/sub (SPILL-ALL t0.f ~325 SPILL then DSP+, FILL t0-arm64.f ~508, DSP+ ~512) where one-cell transfers can use pre/post-index (PUSH t0-arm64.f ~496 already does): 0.51G of the zz run gap plus part of the join spills.
