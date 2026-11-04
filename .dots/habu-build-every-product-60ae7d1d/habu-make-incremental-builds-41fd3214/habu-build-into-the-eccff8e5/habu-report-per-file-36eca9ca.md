---
title: Report per-file reuse in hb-build
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T13:28:36.052595+02:00"
closed-at: "2026-10-28T16:24:30.101744+02:00"
close-reason: "files counted once each (FILES-N; compiled = FILES-N - REUSED-N). Proofs: check.f --verify-only on 8 changed .f rc 0; hb-build-producer-cache-test, hb-build-test, hb-build-aot-image-test, hb-build-cli-errors-test rc 0; native-file-frame-e2e, native-file-auto-e2e rc 0; two hb-build --engine --report-json runs rc 0: 0/155 then 8/147. Reuse assertions move to habu-gate-the-recompile-2ec826c7."
---

Problem: hb-build --report-json cannot say whether an edit recompiled only what changed: the counts live in the maker (tools/native-definition-cache.f REUSED-N/COMPILE-N, PRINT-COUNTS) and HBB-FINISH-MAKER-RC drops the maker's output on success. Ruling (Joel 2026-10-27): report gains files_reused and files_compiled, counting the application-load session only; no schema version change. Design: /Users/joel/.cache/tmp/carl-file/design-keys4.md part (c) (sidecar path in HABU_BUILD_COUNTS beside hb-aot-got.inputs under BF-TMP, read after each successful maker; 0/0 when no maker ran) - part (b) is dropped. Acceptance: an unchanged rebuild reports files_compiled 0; a one-file edit reports files_compiled 1; tests asserting the report shape updated. Verify: rows asserting maker_hit (rg 'maker_hit' tools test), hb-build-producer-cache-test, hb-build-aot-image-test. Depends: habu-delete-the-artifact-1f314ab1 (it rewrites the report). Ownership: carl. Claim: unassigned.
