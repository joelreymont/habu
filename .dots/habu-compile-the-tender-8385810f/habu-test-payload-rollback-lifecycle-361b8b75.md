---
title: Test rollback against the portable payload lifecycle
status: closed
priority: 2
issue-type: task
created-at: "2026-09-13T21:38:50Z"
closed-at: "2026-09-14T00:50:27.213648+03:00"
close-reason: Independent root review approved ef34273b/044d18a1. Actual F and G2 tests pass both tiers; stale-membership control triggers four assertions. Frozen names/effect graphs and live coverage are preserved.
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
