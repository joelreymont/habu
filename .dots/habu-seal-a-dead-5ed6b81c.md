---
title: "Seal a dead quotation's return row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:58:57.976332+02:00"
---

Problem: src/core/checker.f QUOT-RSEAL (:20509) returns at once when CHECK-RETURNS? (:20500) is false, a path that is dead (DEADP) with no exit taken, so a quotation whose body never returns keeps its dead path's return row: `: MKQ [: 1 >r 5 throw ;] ;` infers a quotation whose return row out holds a cell its in does not. Since the row-naming commit (C2a, wplxqwny) REC-SIG refuses such an effect ('effect not recorded', RROW-UNSAID) and every caller then fails E-UNDEFINED, where master loaded it (C2a review F1, repro /tmp/rev-c2a.41203/p1/q1.f). A quotation that never returns says nothing about the rows it leaves; its output rows should carry no constraint from the dead path. Acceptance: MKQ and USE in q1.f load; a quotation that pushes onto the return stack and returns is still refused (QUOT-RSEAL's case 3); a rejected-program test shows the refusal kept and the program shows the dead case accepted. Files: src/core/checker.f, a rejected-program test. Verify: candidate build; those programs; bin/hb --load test/run.f -- checker-soundness. Depends: C2a landed. Ownership: checker.f QUOT-RSEAL. Claim: unassigned.
