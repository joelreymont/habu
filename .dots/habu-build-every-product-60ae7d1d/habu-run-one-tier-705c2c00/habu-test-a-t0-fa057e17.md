---
title: Test a T0 unit past 4096 bytes and shadowed locals
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:44:50.043954+02:00"
---

Problem: two T0 (tier 0 compiler) failures pass the suite. (1) T0's arm64 code buffer starts at SINK-START-BYTES, 4096 (src/habu/t0-arm64.f:119), and W32+ grows it per instruction (:126-127, BUF:GROW); no test compiles a unit past 4096 bytes, so a pointer into the buffer kept across a grow goes unseen (a review mutant of that kind died with SIGSEGV on an 8860-byte body). (2) No test defines a local shadowed by a later local of the same name, so a lookup that finds the oldest one goes unseen. The emission rows and test/t0-install.f the earlier plan extended are deleted (6cffb6cc4b, 7aece1e48f; the suite run through T0 is the test), and the body generator scripts/big.tmpl is gone.
Acceptance: two end-to-end cases through the ordinary load: a definition whose code passes 4096 bytes, with an `if` whose branch is patched past byte 4096 (for example `dup 0< if` then about 1100 `1+` then `then`), runs and answers its expected value; `: L3 ( n n -- n ) {: ab:n :} {: ab:n :} ab ;` on 5 7 answers 5, the newer group's ab (the first group takes the top 7; the audit's arm64 engine answers 5, an oldest-first lookup answers 7). Each case is seen failing against its mutant first.
Files: test/local-spelling-suite.f for the locals case; a suite test file that compiles through T0 for the large unit.
Verify: bin/hb --load on each changed test file, then each with its mutant applied.
Depends: none.
Ownership: unassigned. Claim: unassigned.
