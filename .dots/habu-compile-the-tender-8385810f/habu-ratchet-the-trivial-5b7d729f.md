---
title: Ratchet the trivial-definition compile floor
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.286337+03:00"
---

Problem: 100 definitions of the form : Tn ( n -- n ) 1 + ; take 9.09 ms each through the optimizer (6.83 ms for a three-op body without a combine rewrite); the JIT takes 0.07 ms. Acceptance: a test tool that compiles the 100 trivial definitions through NCOMP:COMPILE and reports the mean, and a ratchet that fails when the mean exceeds the recorded floor; floor recorded at each landing of a sibling dot until it is under 0.5 ms. Files: tools/ or test/compiler/. Verify: run the tool on 41df9051 (about 9 ms) and after each landing. Depends: none. Ownership: rowan. Claim: unassigned
