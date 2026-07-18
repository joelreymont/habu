---
title: Adjudicate stranded evaluator-transaction engine
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:35:34.574388+02:00"
---

Workspace forensic sweep 2026-07-18: workspaces eval-txn (tip 13a229b9) and eval-xref (tip 7ce90836) hold a July 14-15 evaluator/checker transaction engine that never landed and has NO governing dot: src/core/eval-txn.f + src/core/eval-txn-seal.f (package CHECKER-TXN; words EVALUATOR-BEGIN, EVALUATOR-END, EVALUATOR-COMMIT, EVALUATOR-ROLLBACK, DISPATCH-CATCH) plus about 23 test/eval-txn-probe-* fixtures and test/eval-xref.f (an EVALUATE truncation journal). Master has none of it; the satellite claim workspaces from the same campaign carried only lessons and were deleted as superseded. Task: adjudicate like the retired option promotion - rebase the tips onto current master in a scratch workspace, determine whether the transaction engine solves a problem the current checker still has (rollback across evaluator dispatch was the campaign's theme; check whether the landed TDECL/PF transaction work made it redundant), then either route it to landing through review or retire it with a written reason here. The two workspaces must not be deleted before this adjudication.
