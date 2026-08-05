---
title: Remeasure the Linux build-size baseline
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:29:50.018046+02:00"
---

CG-05, tip red. test/gate-build-size.f:89 records BASELINE-LINUX = 123072 — a stale merge resolution from cd7bf8eb; the campaign carried the measured 127168 from ed5d0442 through 5f8e27a7 and the rebuilt exact-tip bin/hb is 127168 bytes. gate-build-size and size-attribution are both red. Fix: remeasure the exact integrated tree, attribute the growth honestly, and make the two size owners agree. Do not infer attribution from source-line counts or page rounding.
