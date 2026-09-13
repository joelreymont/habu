---
title: Test rollback against the portable payload lifecycle
status: active
priority: 2
issue-type: task
created-at: "2026-09-13T21:38:50Z"
blocks:
  - habu-compile-the-tender-8385810f
---

Owner: cedar-indexed-dictionary. The old signature-pool fixture reads row
counts before freeze and treats the live membership marker as a serialized
offset. It now uses CHECKER-PAYLOAD-ARM/FREEZE/LOOKUP/SPANS, checks live
membership/missing coverage across actual candidate-scope rollback, and reads
frozen names plus verified effect graphs. Exact frozen row counts are 2, 0
and 1: only surviving captured symbols serialize. Known uncovered symbols
still answer row zero, the condition the capture audit refuses.

Both tier0 and tier1 focused loads pass on the actual F product. A process-local
control redirects ASIG-SYMS-RETIRE to a same-effect no-op; the fixture fails
four assertions for reused symbols (missing coverage and frozen row zero).
No production code or binary file is changed. Logs:
/tmp/cedar-rollback-{after,tier1,stale-control}.{out,err}; control source:
/tmp/cedar-rollback-stale-control.f. Independent review remains before landing.
