---
title: Let drop remove a stale option after catch
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:26:14.850888+02:00"
---

Problem: docs/forth.md:2637-2639 says a multi-cell value left stale by a catch is one logical stale value that one drop removes. The checker refuses that drop with an empty reason: /tmp/stuart-habu/stale3.f (stuart, Tender, 2026-11-07; installed hb at master 2dbfb33041) defines OPTN, '1 2 3 option:SOME ['] KEEPO catch drop drop 2drop' with KEEPO ( n n option<n> -- n n option<n> ), and OPTED with option<PK:thing>; both fail at the second drop ('hook: non-certified definition: optn at drop', ncomp exit 70) while PLAIN with three n compiles. Cause not yet found. Acceptance: the root cause is fixed in the layer that has it (checker, elaboration or compiler); OPTN and OPTED compile and run, and a refusal of a typed use of the stale option (E-STALE-READ) still stands; the refusal path, if any remains, names its reason. A rejected/accepted program pair in the checker's tests covers it. Files: to be named by the root-cause report. Verify: bin/hb --load /tmp/stuart-habu/stale3.f prints done; the checker test that gains the case. Depends: none. Ownership: carl (Habu integrator); Tender lane x2b waits on it. Claim: unassigned.
