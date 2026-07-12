---
title: "maki: concat-backward integration test (BW-STEP-CONCAT path)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T19:35:57.896030+02:00"
---

Audit review LOW-1, pre-existing gap: no test runs BW-BUILD over a CONCAT model, so the concat-backward emission path (backward.f BW-STEP-CONCAT: ZERO-ROWS/ROWS+/MV-PACK-ROWS/MV-SLICE-VD in situ) has only unit-level coverage. Both BW-SL row-range args are CAD-KIND:rows, so an r0/r1 transposition is checker-invisible - only an integration pin catches it. Add a concat-backward MODEL to maki/backward-test.f asserting the two emitted slice/pad-scatter nodes' row params (e.g. concat 2x4 + 3x4 -> slices [0,2) and [2,5)).
