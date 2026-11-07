---
title: "C2c3: write the facts of defers, casts, created words and definers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T23:55:34.819909+02:00"
blocks:
  - habu-c2c2-store-a-34061842
---

Problem: DECISIONS 'where a word's facts text is laid': besides colon definitions (C2c2), the checker learns a word's facts at a defer's registration, a cast: or linear: declaration, a top-level create, variable or constant (TRUST-RAW, which runs while the created body is open on both loops: definers.f DEF-ADDR, habu2.f EMIT-CREATE), and a does> definer's publication. Acceptance: TRUST-DECL and CHECKER-DEFER (with the defer tag) and CHECKER-DEFCAST write through CHECK:FACTS!; TRUST-RAW renders the created text, looks it up with INTERN:FIND ( ptr u8 n -- n ), 0 when absent, and stores its place or leaves 0, never laying; CHECKER-NATIVE-DOES-PUBLISH writes the definer's text once and lays its creates-effect tail as a string of its own. Acceptance cases: `variable V`, `create B 100 allot` and a definer's `DEF FOO 0 ,` keep their first cells; a defer, a cast: word and a definer get [32]. Files: src/core/checker.f, the INTERN early file. Verify: candidate build within 5 minutes; a probe reading [32]; chain fixpoint; bin/hb --load test/run.f. Depends: C2c2. Ownership: those files. Claim: unassigned. Note (C2c grow review, 2026-11-07): SIG-CREATES-A (checker.f, set by the signature parser) points into RSBUF and FACTS-CLEAN does not clear it (no reader today); a writer must not hold it across an RSBUF write.

From C2c2 r3 (2026-11-07): every writer stores through CHECK:FACTS!, whose last step is the kernel primitive record-facts! (a plain global row ( n n -- ), refusing a sealed record; a plain store into a record faults). C2c3's acceptance case `create B 100 allot` must also pass for the six storage definers' generated `create ... allot` (lane jerry-lbuf).
