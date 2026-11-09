---
title: "Measure STEP's load and run cost"
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.128506+02:00"
---

Problem: NAME-STEP adds a second generated word per table; its cost in generated size, load time and per-dispatch call overhead is unmeasured.
Acceptance: recorded in this dot: the generated size and load time of a 100-row table (a copy of Tender's HEADER-BUILD, src/rules.f:352 region) before and after NAME-STEP; the throughput of one CPU-bound scanner on a representative input before and after, with byte-identical output. If the per-dispatch call costs materially, emit the shared arm representation into both words and record the numbers again.
Files: a scratch table and scanner under the ignored scratch directory; src/lib/fsm.f only if the re-emission is needed.
Verify: the recorded numbers and the identical outputs.
Depends: Generate NAME-STEP beside the runner.
Ownership: measurement only; src/lib/fsm.f if re-emission is needed. Claim: unassigned.
