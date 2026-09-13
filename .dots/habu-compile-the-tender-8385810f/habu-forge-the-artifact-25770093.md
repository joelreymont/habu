---
title: "Exercise real artifact reader refusal branches"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T10:53:21.388233+03:00"
blocks:
  - habu-preserve-complete-addr-258c0288
  - habu-wire-the-checker-eec26aea
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own aot-chain-capture-suite.f negatives and artifact header documentation;258c0288 owns correct rows/MERGE, eec26aea payload. Mutate actual artifact for truncated payload, malformed section table, changed chain source, version and producer key. Assert existing named error/child exit. Begin with independent correct rows/payload, not write-read-write equality. Correct nonexistent-test claims. No generalized fuzz framework.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Additional source-confirmed boundary: AOT-FILE:?TABLE formerly added a positive section length to CUR before checking PAYLEN, allowing signed overflow. The owned-capture implementation checks length against remaining payload before addition; add an overflowing-length artifact negative to the existing malformed-section acceptance. No invalid memory access was executed.

That boundary and its negative are integrated at `2b94ee43`: a positive,
cell-aligned near-MAX section length receives `section scalars runs past the
payload` (75) before copying. The registered artifact suite passes on lifecycle
host `cdac89e02a34`, including real owned capture restoration after every original
section is erased and its large source mappings are released. Other malformed
header/source and checker-payload cases remain in this task.
