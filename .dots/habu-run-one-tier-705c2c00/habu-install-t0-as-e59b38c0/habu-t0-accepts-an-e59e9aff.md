---
title: T0 accepts an uneven if/else join that tier 1 refuses
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.340133+02:00"
---

Problem: : HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ; under 0 set-check: tier 1 refuses at then with -8503; T0's RECONCILE (src/habu/t0.f:815-824, from DO-THEN :856) absorbs the depth difference and the program runs on to E-EVAL-RESIDUE -3804 (program .jj-ws.noindex/jerry-s2/.out/gaps/uneven.f; suite sort class d, 2026-10-30). Acceptance: T0 refuses the join as tier 1 does, a failing test first. Files: src/habu/t0.f. Verify: the program refuses with -8503. Depends: none. Ownership: t0.f control-join path. Claim: unassigned.
