---
title: Keep binding glue state local
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:28:25.738663+03:00"
closed-at: "2026-10-20T18:19:58.068532+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Both split/merge native-file-auto segment image-equality regressions on21b4 differ at checker BGLUE-I (DATA4729808,1warm versus2fresh); semantic dictionary/runtime/counters match. BGLUE-I and BGLUE-MASK are confined to one BIND-ROW-GLUE invocation; only the returned mask escapes. Carry loop position and mask as local/stack state, remove private globals, preserve exact term/width/error/64-bit mask behavior and consumers BWIN-COMMIT/SELECTED-CALL-ABI. Existing segment image-equality failures precede code. Verify existing binding/wide-layout E2Es, unchanged generation/equality assertions, matching native/fullsuite/convergence and independent review. No new capture scrub entry or weakened comparison.
