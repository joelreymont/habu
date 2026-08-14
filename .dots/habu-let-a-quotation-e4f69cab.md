---
title: Let a quotation function return under a tail contract
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.819331+02:00"
---

Found by the sel-tail landing: regalloc-verify.f VTAIL-CK asks the emission's contract of EVERY function, so a quotation function's ordinary return inside a definition whose function zero declares a tail call is held against that tail contract and refused E-A64RAV-SHAPE (-8339). Before the landing the selector refused the shape earlier (-8620); now the selector is right and the verifier is the wall. No miscompile - a lost optimisation, pinned live in test/compiler/native-tail.f QUOT-CASE with prose saying the row changes when this lands. Fix: the verifier's tail clause becomes per-function (function zero's contract speaks for function zero). Files: src/compiler/native/regalloc-verify.f. Depends: none.
