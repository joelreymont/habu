---
title: Release checker local scan workspace
status: closed
priority: 2
issue-type: task
created-at: "2026-10-17T00:09:47.159359+03:00"
closed-at: "2026-10-20T18:19:58.278534+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Fresh d2 literal E2E F18/F32 differ first in LOCNB and retain local/control cursors. Make LCO, LROW/LCH and CTMP/RTMP invocation-owned; retire local tables and the existing boot width arena at checker quiescence after native lowering and diagnostics finish borrowing results. Preserve checker ABI and authority; no CWORK import into the early core or central scrub list. Existing literal equality and restored typed/wide-local acceptance/refusal cover behavior.
