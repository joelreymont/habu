---
title: Checked code can install a buffer as its own data stack
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T23:17:49.026361+02:00"
---

The aliasing hazard behind the DKEEP policy ruling (merged 6d171a2d; evidence on 2fa72257's archived leaf): run-in-stack is a CHECKED prim (checker.f:5734 (was 5369 pre-signature-campaign)) handing an ordinary MEM-ALLOC span to the engine as the data stack (BRUNSTACK x19=base), so a checked program can hold the address of its own live stack slots and store through it. NO policy makes such a program well-defined under a register-allocating compiler - O is merely the faithful-to-the-engine choice (memory authoritative at transfer points). Decide whether the checker should constrain run-in-stack's span (a fresh-allocation-only rule? an opaque handle?) or whether the documented O semantics are the contract; either way the answer lives at DOUT-AT's prose and run-in-stack's axiom row so the next reader does not re-open it. Files: src/core/checker.f, src/compiler/native/select.f prose. Depends: none.
