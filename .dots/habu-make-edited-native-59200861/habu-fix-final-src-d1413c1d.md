---
title: Fix final source tooling checks
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.216391+03:00"
closed-at: "2026-10-20T18:19:57.579273+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Reproduced the 12 LSP, four use-count and grown-file range failures on fixed-point a5f93a78/9cfc53ca. Completed disk reads now retain acquired bytes for diagnostic ranges while navigation still requires stable metadata, reusing the inspected a5269af6 fix. The other expectations were stale: opaque top-level calls defer include.f, roles.f, cell.f and pointer-storage.f indexes, and real require/include/TRUST bindings add use packets. Existing constructor locations and known references remain covered. whole-source-read, check-verify and both lsp-boundary files exit 0 with external scratch, stdin at EOF, ulimit 512 and caffeinate; logs: /Users/joel/.cache/tmp/carl-source-gates.U7cEcj. Baked code, origin ABI and index storage are unchanged. Independent review and integration remain; keep this task active.
