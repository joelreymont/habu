---
title: Measure the generator changes
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:10:47.128506+02:00"
blocks:
  - habu-load-the-consumers-e9f13340
---

Problem: NAME-STEP (fd307801) and the callback-effect hold (9f6510cc) landed unmeasured. The hold emits one generated word per action line and per FINAL (src/lib/fsm.f IN-ROW 166, FINAL-ROW 184), and the runner now calls NAME-STEP once per dispatch; what this costs a consumer in generated size, load time and run time is unknown.
Acceptance: recorded in this dot, b7273a0's src/lib/fsm.f against master's on the same hb and machine: Tender's load time (tender.f) and generated size, and the run time of one CPU-bound scanner on a representative input with byte-identical output. This dot changes no code: a cost that matters gets its own dot naming the number.
Files: none; scratch under the ignored scratch directory.
Verify: the recorded numbers and the identical outputs.
Depends: Load the consumers on master's FSM generator.
Ownership: measurement only. Claim: unassigned.
