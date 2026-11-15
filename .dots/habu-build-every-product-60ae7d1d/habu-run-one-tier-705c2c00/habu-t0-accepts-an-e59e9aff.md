---
title: T0 accepts an uneven if/else join
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.340133+02:00"
---

Problem: T0's RECONCILE (src/habu/t0.f:803, from DO-THEN :844-849) absorbs an uneven if/else join: it takes no depth check, so a body compiled without the checker that joins arms of different depths runs on. The reproducer below no longer reaches it: `0 set-check` turns no checking off (docs/forth.md "set-check turns no checking off"), and on the installed hb the checker refuses the definition at `then`:

    0 set-check
    : HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ;

prints `habu: in hr-uneven: at 'then' the if and else arms leave different stack depths expected: n actual: n n` and `ncomp: cannot compile HR-UNEVEN at then`, rc 70. Not known: whether any body reaches T0 without the checker (the candidates are the files src/habu/boot-files.f loads below the checker). Acceptance: T0 refuses an uneven join in a body the checker does not see, a failing test first. Files: src/habu/t0.f. Verify: the test refuses at `then`. Depends: none. Ownership: t0.f control-join path. Claim: unassigned.

A depth check in RECONCILE is not the fix. RECONCILE's count is of virtual-stack entries in registers, not the data stack's depth: a call spills the whole virtual stack (SPILL-ALL, :255), so two checked arms of equal depth arrive with different entry counts (`: F ( n n bool -- n n ) if H else 7 + then ;` arrives with one). SINK-ENTRIES (:788) and RAISE (:793) exist for that. A sound refusal needs the real depth, register entries plus cells spilled to memory, recorded at SNAPSHOT (:728) and compared at RECONCILE: a memory-depth counter kept across spills, fills and calls (a call's in and out from its record).
