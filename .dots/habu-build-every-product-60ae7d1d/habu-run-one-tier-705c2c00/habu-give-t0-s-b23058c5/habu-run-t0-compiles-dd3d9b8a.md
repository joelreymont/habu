---
title: "Fix the card's lease clause"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-02T19:51:43.958878+02:00"
closed-at: "2026-11-03T10:12:13.190862+02:00"
close-reason: "card clause fixed in ppoxnkmm, landed as master b28bd8e8 (lzxusxpr 63bd6714 S1, momzzlnr 579282ab S3, ppoxnkmm b28bd8e8 card) on 04955959; code-reviewer REVISE fixes folded; gate on the stack: candidate rc 0 (compiled 15635), full arm64 suite one red lsp-boundary (30 s child deadline at load ~135, passes alone; same red on carl's master run, dot habu-lsp-entry-test-4d67fa4a), two-generation chain gen 5 = gen 4, x86-64 cross-build rc 0; no file overlap with batch 2; its open NSHADOW:OPEN/CLOSE finding moves to habu-remove-the-compiler-d44fac90"
---

Lane S2 of /tmp/jerry-big/t0state-design.md is dropped (Joel 2026-11-03, LESSONS.md "the compiler lease"): T0 takes no lease, M-OPEN and NDEFINITION:IDLE-CK stay, no public word changes. Left: docs/forth-card.md:487 names `NLEASE:IDLE?`, which does not exist (lease.f has IDLE-CK); remove that clause (a doc fix, foldable into the S1/S3 landing). Open finding, not yet a lane: NSHADOW:OPEN and CLOSE (shadow.f:290, :316), backend.f:334 and NCOMP:CAPTURE-PREPARE ask NLEASE:IDLE-CK, which a T0 compile does not hold, so they refuse during a tier-1 compile but not during a T0 one; only code run inside a compile (a publication observer) could reach them. Decide whether they ask NDEFINITION:IDLE-CK instead (load order: shadow.f vs definition.f) or the observer contract (compiler.f:126-128, must not allocate or throw) is enough. Claim: unassigned.
