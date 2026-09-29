---
title: Fix stripped-image merge review findings
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T21:56:04.586840+02:00"
---

Review of 6ecf20a5 (Build stripped test subjects as one image) left two MEDIUM findings. (1) Entry refusals moved maker-direct (test/stripped-entry-lib.f:77-91), so no row asserts that the hb-build tool passes the maker's exit 74 and 'aot: entry word not found:' through unchanged. d6ba5300 then moved hb-build fixtures in process; check tools/hb-build*-test.f for any remaining CLI pin of maker refusal propagation. If none, keep one of MISSING-GLOBAL / PRIVATE-REFUSED (or one existing CLI call) on the hb-build BUILD path asserting rc 74 and the message. (2) test/stripped-image.f:15-19 IMAGE-MAX comment describes the sparse subject but bounds the merged image (148,860 bytes measured); restate what the bound proves. LOW: stale reference to deleted test/compiler/aot-xt-cells.f at tools/object-image.f:33 (the aot-data-cell-refusals.f references belong to the speed-up-aot dot). Acceptance: a mutation that makes hb-build swallow or rewrite the maker's rc/message fails a row; comments match the code; touched rows pass standalone. Files: test/stripped-image.f, test/stripped-entry-lib.f, tools/hb-build-test.f (one CLI case), tools/object-image.f comment. Depends: none.
