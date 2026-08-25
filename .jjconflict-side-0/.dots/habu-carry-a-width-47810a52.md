---
title: Carry a width per token in the inline row
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T08:20:40.371166+02:00"
---

Found by the wide-load lane: a recorded body may not hold a wide memory access - inline.f records spellings and kinds, not source offsets, so a spliced row would elaborate against the caller's width facts. SPLICEABLE? refuses by name (named, tested boundary; loses an optimisation, never an answer). Retirement: the inline row learns a width per token (or carries the offset mapping). Sits beside inline.f's two existing open refusals (locals, control structures) - same shape, take together. Files: src/compiler/native/inline.f. Depends: none.
