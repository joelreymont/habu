---
title: Reject a wide value at ptr-cell!
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T07:22:52.224750+01:00"
closed-at: "2026-11-18T08:17:34.674975+01:00"
---

Problem: ptr-cell! of a str is admitted but stores one cell: src/core/checker.f:16922 maps ptr-cell! to INTRINSIC-CELL-STORE, so check-walk.f:238 runs CELL-STORE-TOK with layout true (a whole-value store), while the primitive (src/habu/prims.f:417) and T0 (t0.f OPS-MAP :1705-1720 maps only !) move one cell. Reproducer: TYPED-VARIABLE PV str  : L ( -- n ) PV @ str> nip ;  : S ( -- ) s" abcde" >str PV ptr-cell! ;  S L . cr prints 0 then E-EVAL-RESIDUE, rc 70, in a body and at top level. Acceptance: ptr-cell! of a value wider than one cell is rejected at check time as xt! and atomic! are (E-MISMATCH, the one-cell reason); a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ptrcell.
