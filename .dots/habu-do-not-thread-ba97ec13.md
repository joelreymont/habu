---
title: Do not thread a memory order through a call that touches none
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T15:02:23.178171+02:00"
---

src/compiler/native/select.f CALL-SAVE/CALL-RESTORE thread the memory-order token through every call site, including one whose callee cannot touch memory. Since dot habu-narrow-what-a-5d6a0845 landed, the callee's own emission is the authority on what it does - the same seam that records which registers it destroys (src/compiler/native/clobber.f) could record whether it reaches memory at all, and a call to a routine that does not would need no order threaded. MEASURE FIRST: the token holds no register (A64RAV:REGGED? is false for the memory type) and nothing schedules yet, so today the threading costs zero instructions and the change would buy nothing measurable - it is worth doing when a scheduler exists and the order becomes a real constraint, and it should be justified against the committed tables at that point rather than now. Owners: A64SEL, NCLOB.
