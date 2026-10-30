---
title: T0 accepts an uneven if/else join that tier 1 refuses
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.340133+02:00"
---

Problem: : HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ; under 0 set-check: tier 1 refuses at then with -8503; T0's RECONCILE (src/habu/t0.f:815-824, from DO-THEN :856) absorbs the depth difference and the program runs on to E-EVAL-RESIDUE -3804 (program .jj-ws.noindex/jerry-s2/.out/gaps/uneven.f; suite sort class d, 2026-10-30). Acceptance: T0 refuses the join as tier 1 does, a failing test first. Files: src/habu/t0.f. Verify: the program refuses with -8503. Depends: none. Ownership: t0.f control-join path. Claim: unassigned.

2026-10-30 14:05 (jerry-g2, abandoned): a depth check in RECONCILE is not the fix. RECONCILE's d counts virtual-stack entries in registers, not the data stack's depth: a call spills the whole virtual stack (SPILL-ALL), so two checked arms of equal depth arrive with different entry counts (test/t0-install.f:436 TWIN-HITS, d=1 at then; `: F ( n n bool -- n n ) if H else 7 + then ;` d=1). SINK and RAISE exist for that. A sound refusal needs the real depth, register entries plus cells spilled to memory, recorded at SNAPSHOT and compared at RECONCILE: a memory-depth counter kept across spills, fills and calls (a call's in and out from its record). Deferred: unchecked bodies only; tier 1 keeps refusing the suite row at `1 set-tier`.
