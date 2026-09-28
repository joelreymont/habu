---
title: Intern checker symbol name strings
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-28T13:31:39.020637+02:00\\\"\""
closed-at: "2026-09-28T15:12:38.806545+02:00"
close-reason: Interned checker symbol strings while preserving identities, visibility, effects, offsets and rollback. Source c17efeda passed independent review, native B2–B5 and sidecar equality, all 492 suites, two saved-image replays and unchanged Maki board hashes with strict signatures. Engine 2790775 bytes, SHA614b033636dab64e95d6195ccb29c8c08d6463765dbd625e603cd9cb2cba1c0c, down16512; DATA values-16804, bitmap-192, code+632. Runtime index adds512KiB per mapping; inherited reset does not unmap. One build pair54.95s/54.85s is informational. Receipt ~/.cache/tmp/habu-name-intern-completion-20260928-02.md.
---

Current sourcee337d11f/B5 SHA24003f has168771 bytes of row-referenced symbol strings; collision-resolved physical census finds14787 additional same-role spelling bytes plus11 cross-role bytes. Intern same-role names at SYM-NAME!, preserving every symbol identity/effect/horizon and existing offsets. Keep composite identity hash; add two process-local HIDX name bucket/next planes with exact folded byte comparison and existing append/LIFO retirement/rebuild/reset lifecycle. Extra current-capacity mapping is512KiB runtime scratch, not captured state; existing reset drops mappings without unmapping, so report actual lifecycle tradeoff. No pool compaction, cross-role sharing, symbol pruning, ABI/format or capture checkpoint change. Establish actual two-image capture, distinct package/visibility effects, rollback/new name reuse and source reconstruction E2E before code; reuse existing growth/index suites. Measure fresh native pool/DATA/code/file delta and build timing, native convergence/full gate/Maki. Design ~/.cache/tmp/habu-symbol-name-intern-design-20260928-01.md; audit ~/.cache/tmp/habu-captured-data-audit-20260928-01.md. Lead owns dot/integration/review; Sol implementation. Heavy qualification serialized after native scalar fix.
