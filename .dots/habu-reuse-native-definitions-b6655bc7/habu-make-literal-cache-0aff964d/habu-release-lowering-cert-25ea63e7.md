---
title: Release lowering certificate scratch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-17T00:32:54.918708+03:00"
closed-at: "2026-10-20T18:19:58.348527+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Current d2 cold/warm engine comparison has163 differing cells in declared LOWER-CERT boot tables, not DBUF-W. Scope src/core/layout-valid.f: invocation-local queue cursors and retire DATA/ENV/GUARD/TASK/FETCH boot storage at existing REG-CERT-SNAP-XT after pass2/report borrowers. Preserve live BUF until owner quiescence. Use existing lower-cert, lower-txn-large, layout-valid-growth and type-layout-lower-pending behavior; require matching cold/warm images and final native/convergence checks.
