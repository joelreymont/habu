---
title: Give the JSON writer a caller-owned buffer
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:58:32.427303+03:00"
---

Problem: lib/json-write.f writes into one process-wide buffer (JW-BUF-A, JW-OUT-LEN), so two tasks writing JSON interleave into it; Tender's server serialises every JSON-WRITE reset/fill/copy under a TASK:FACILITY to cope (aspen, 2026-09-17). lib/json-read.f already takes the reader's storage from the caller. Acceptance: the writer takes the caller's buffer (a span the caller owns, or a writer handle minted over caller storage the way json-read does), the process-wide buffer goes, every in-tree caller converts, lib/json-write-test.f covers two writers alive at once with different buffers, and docs/stdlib.md and the module header say the state is caller-owned. Files: lib/json-write.f, its test, the callers (rg -l JSON-WRITE lib tools test src). Verify: json-write tests, the callers' suites, test/run.f. Depends: none. Ownership: lib/json-write.f. Claim: unassigned.
