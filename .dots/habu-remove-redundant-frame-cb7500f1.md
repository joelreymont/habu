---
title: Remove redundant frame shuffling
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.677506+03:00"
---

Otto’s codegen report measures29,552 bytes of frame-slot traffic including1,186 adjacent copies (9,488 bytes). Establish which copies are redundant using compiler facts, not instruction-pattern deletion alone; preserve aliasing, wide values, spill/call and failure behavior. Estimated10–13KB savings are unverified. Acceptance: same-source executable behavior and emitted-code/size measurement; affected native E2E, full native gate, convergence and Astra review. No compression or application edits.
