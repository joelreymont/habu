---
title: "Check the C2 owner's disposer"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T14:27:01.609387+02:00"
closed-at: "2026-10-31T17:13:33.912965+02:00"
close-reason: "Landed d3bfd45a 'Check the C2 owner's disposer': the view-representation cast admits the callback pack ( [ V -- ] -- [ ptr u8 n -- ] ) in C2-MEM only, and ALLOC-DISPOSE is a checked : through >CELL-DISPOSER and >ALLOC-DISPOSER. TRUSTED: in src 45 -> 44 (c2-owner.f 1 -> 0). Proof: same gate as habu-pass-a-fresh-bb992e31 (suite 510/510, two-generation, x86-64 cross-build); c2-reopen-refusals ok on the whitebox engine; CNV10 fails on a scratch build that misroutes a callback pack and passes on the final one; c2-owner-dispose output byte-identical to master's; Fable feature review LAND on behaviour."
---

Problem: src/lib/c2-owner.f ALLOC-DISPOSE is TRUSTED: because it hands a view-typed disposer [ mut-view<p,p,fresh-region-b,u8> -- ] to c2-memory.f APPEND's raw callback [ ptr u8 NUM:alloc-byte-len -- ]; a private CAST: of the callback is E-CAST-SCOPE and a trusted leaf loses the fresh region (census tc1, MODEL group 1). Acceptance: ALLOC-DISPOSE is : through the smallest sound change at its owner (library or VIEW-CAST-CERTIFY), with rejected and accepted cast rows; or it stays TRUSTED: with the measured reason in its comment. Verify: engine build; rows c2-memory, c2-memory-runtime, c2-memory-source, c2-memory-aot, c2-init-accessors, c2-init-accessor-image, cast-negative. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-c2cb

Waits on habu-pass-a-fresh-bb992e31. The first worker stopped undecided, nothing committed; @ in jerry-c2cb holds "WIP: Check the C2 owner's disposer" (a callback pack `( [ V -- ] -- [ ptr u8 n -- ] )` admitted in C2-MEM beside VIEW-CAST-CERTIFY, private casts >CELL-DISPOSER and >ALLOC-DISPOSER in c2-owner.f, rows in test/c2-reopen-refusals.f; unproven). The build refuses ALLOC-DISPOSE at >CELL-DISPOSER with MD-SCOPE-KIND: a declared fresh-region view cannot reach a region-generic word (P5 in the blocking dot). Handoff /tmp/jerry-big/c2cb-handoff.md. When the blocker lands, a fresh worker rebases the WIP onto it and finishes.
