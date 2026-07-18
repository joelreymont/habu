---
title: "M4: bulk side-content scanner"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:36.756504+02:00"
blocks:
  - habu-m3-authenticated-modular-ed51c697
---

Start only after M3 is closed on green master. Own one checked jj external-diff scanner invocation that binds ordered metadata rows to left/right path bytes and kinds, uses the checked no-follow outcome, hashes complete content, classifies binary bytes, emits deterministic HABUSIDE framing, rejects escape/corruption/truncation, and proves constant child-process count. Existing owner: habu-tools-bulk-diff-f36d0508. Finish by building the real scanner through hb-build, registering its focused/real-jj gates, independently reviewing it, and promoting the green milestone to master.

RECOVERY POINTER 2026-07-18 (workspace forensic sweep): the diff-infrastructure implementation for M4/M5 exists only in held workspaces, never on master: tools/diff-capture*.f (package DIFF-CAPTURE-CLI + metadata/content/transaction/frame modules), tools/diff-side-content.f (package DIFF-CONTENT), tools/bulk-diff-scan*.f, tools/lint/diff*.f. Fullest tip: workspace habu-tools-frame-diff-e98f8a6a at 123c9567 (28 files); siblings habu-tools-bulk-diff-f36d0508 33a68ed2 (divergent), habu-lint-diff-recover 357906a5, habu-diff-land-side-98dd8f40 5f4d26c8, sol-review-side 1f4ea1a8, sol-review-side-fixes daae137d. Recover from the fullest tip after M3's composer lands; retire siblings after. Do not delete these workspaces before then. (Same pointer applies to habu-m5-framed-change-fa1fd960.)
