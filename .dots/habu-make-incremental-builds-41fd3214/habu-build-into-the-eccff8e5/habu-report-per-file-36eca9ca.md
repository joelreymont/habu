---
title: Report per-file reuse in hb-build
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:28:36.052595+02:00"
---

Problem: hb-build --report-json cannot say whether an edit recompiled only what changed: the counts live in the maker (tools/native-definition-cache.f REUSED-N/COMPILE-N, PRINT-COUNTS) and HBB-FINISH-MAKER-RC drops the maker's output on success. Ruling (Joel 2026-10-27): report gains files_reused and files_compiled, counting the application-load session only; no schema version change. Design: /Users/joel/.cache/tmp/carl-file/design-keys4.md part (c) (sidecar path in HABU_BUILD_COUNTS beside hb-aot-got.inputs under BF-TMP, read after each successful maker; 0/0 when no maker ran) - part (b) is dropped. Acceptance: an unchanged rebuild reports files_compiled 0; a one-file edit reports files_compiled 1; tests asserting the report shape updated. Verify: rows asserting maker_hit (rg 'maker_hit' tools test), hb-build-producer-cache-test, hb-build-aot-image-test. Depends: habu-delete-the-artifact-1f314ab1 (it rewrites the report). Ownership: carl. Claim: unassigned.
