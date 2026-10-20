---
title: Preserve captured deferred FILE state
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T16:53:17.744895+03:00"
closed-at: "2026-10-20T18:19:57.824013+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

STRICT-DEFER originally exported and ran 456, but FILE import refused an ordered source action (-8597): TRANSFER-ROW skipped the exact source occurrence facts when the target held only a primitive effect. It now transfers an ordinary source graph past that symbol's exact active primitive row, retaining primitive call authority and existing target-row/link precedence. Independent Astra source and guard follow-up review are clear. The real minimal FILE export/import passes, with a FILE hit on import; a downstream `( n n n -- ) LDEFER-BIND` still refuses the pointer mismatch after that hit. The unchanged native-file-admission-e2e passes exit0, including456 after deferred layout replay (108.83wall/48.72CPU). These behavior checks use the9891 matching engine before the equivalent sequential-guard simplification. Final combined-source build, native suite and convergence remain required. Implementation workspace and scratch are retired; the integrator owns its copied host pair and final qualification.
