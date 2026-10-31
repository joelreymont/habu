---
title: Compile a 64-cell STRUCTURE maker at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:25:14.212485+02:00"
---

Problem: at tier 1 the maker of a STRUCTURE 64 cells wide fails to compile. /tmp/jerry-dots-probe/st64.f (g2..g32 by doubling, then `STRUCTURE g64 FIELD a g32 FIELD b g32 ;STRUCTURE` after `1 set-tier`) on the public hb prints `g32 ok`, then `ncomp: cannot compile G64-MAKE`, `habu: bad structure declaration 'g64': declaration failed at 'g64'`, `hb: uncaught throw code -8611` (E-A64RAV-DKEEP, src/lib/errors.f:829: the arm64 emission made a data-stack store or load it had no reason to make); the same file at `0 set-tier` prints `g64 ok`. Found by the dtail re-review (/tmp/jerry-dtailrr2-tmp/p/hookless-t1.f). Acceptance: at tier 1 a STRUCTURE of 64 and 128 cells declares and its maker runs; the root cause in the tier-1 arm64 emission is fixed, not the verifier's check relaxed; a row in the tier-1 rows asserts it. Files: the arm64 tier-1 emitter that produced the redundant access (reduce first), the row's test file. Verify: st64.f and hookless-t1.f on the candidate; the tier-1 rows. Depends: none. Claim: unassigned.
