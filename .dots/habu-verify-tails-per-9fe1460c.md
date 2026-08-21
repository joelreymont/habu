---
title: Verify tails per function not per module
status: closed
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:43.445896+02:00"
closed-at: "2026-08-21T09:40:14.207284+02:00"
close-reason: "Tracker GC 2026-08-21, proof exact duplicate. This leaf's body is byte-identical to habu-let-a-quotation-e4f69cab - same failing path (regalloc-verify.f VTAIL-CK holding a quotation function's ordinary return against function zero's tail contract, E-A64RAV-SHAPE -8339, pinned live in test/compiler/native-tail.f QUOT-CASE) and same fix shape (make the verifier's tail clause per-function), same owned file src/compiler/native/regalloc-verify.f. Proof: diff of the two bodies from line 8 is empty. This leaf was filed 24 seconds after e4f69cab (02:43:43.445 vs 02:43:19.819), so the younger closes into the older. The work is NOT done - it is open on habu-let-a-quotation-e4f69cab, which carries the cross-reference."
---

Found by the sel-tail landing: regalloc-verify.f VTAIL-CK asks the emission's contract of EVERY function, so a quotation function's ordinary return inside a definition whose function zero declares a tail call is held against that tail contract and refused E-A64RAV-SHAPE (-8339). Before the landing the selector refused the shape earlier (-8620); now the selector is right and the verifier is the wall. No miscompile - a lost optimisation, pinned live in test/compiler/native-tail.f QUOT-CASE with prose saying the row changes when this lands. Fix: the verifier's tail clause becomes per-function (function zero's contract speaks for function zero). Files: src/compiler/native/regalloc-verify.f. Depends: none.
