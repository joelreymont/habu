---
title: Cost a placement by the move the transfer can carry
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:53:01.197444+03:00"
---

Problem: src/compiler/native/select.f DPLACE-CHOOSE counts one instruction for every required place that is not the chosen one, a cost model written before a data-stack move could ride the transfer beside it (fusion lane, 2026-09-16, docs/compiler-measurements.md section 9). `( -- n n )` is the smallest case it now gets wrong: it stands at 0, the last store lands one cell under the pointer and the publish keeps `add x19,x19,#0x10`; standing at 8 would have cost the same and fused one move. Acceptance: the placement cost counts a move the neighbouring transfer can carry as free, and regalloc-verify.f VDPLACE-CK is changed in the same commit because it re-derives the placement from the module; a fixture in test/compiler/native-fused-moves.f pins the `( -- n n )` shape without the trailing add; tools/tier-census.f bytes at or below the section 9 figure with 0 words grown; byte fixpoint; the native-* suites. Files: src/compiler/native/select.f, regalloc-verify.f, test/compiler/native-fused-moves.f, docs/compiler-measurements.md. Verify: census; suites; fixpoint. Depends: none (354ea082 landed). Ownership: tier-1 selection. Claim: unassigned.
