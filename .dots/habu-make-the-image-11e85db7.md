---
title: "Make the image walker's identity a tiling proof"
status: active
priority: 3
issue-type: task
created-at: "2026-09-18T12:50:13.928290+03:00"
---

Problem (adversarial review of the 2026-09-18 batch): tools/image-size-lib.f ELF-ROWS splits the header page with LAST-NONZERO (elf/dynamic-metadata 189 bytes on bin/hb — not a multiple of 8, so the last field's trailing zero bytes are billed to the pad), the same value heuristic the stripped walk replaced with a structural row count; and ROW takes an independent (start, len) per class while SUMS? compares only TOTAL with ILEN, so a shape that double-counts one region and omits another of equal size reports a clean identity with wrong attribution and a wrong zero column. Acceptance: the header page split read structurally (the ELF program headers and the dynamic metadata's own lengths), and a monotonic cursor check — each row begins where the previous ended and the last ends at ILEN — so the sum-to-length identity is a tiling proof; the engine report's numbers re-derived and stated (they may change for the header rows); the adversarial tampers still refused. Files: tools/image-size-lib.f, tools/hb-build-test.f, docs/engine-size.md. Verify: the tool on bin/hb, a --repl image and a stripped image; tools/hb-build-test.f; test/run.f. Depends: habu-report-where-an-cdcd7976 (worker 2 landing). Ownership: build tools. Claim: agent=alder workspace=.jj-ws/alder-image-tiling-final, based on c921e431.

Implementation keeps the current data-cell-bitmap/value rows, derives ELF
metadata through PT_INTERP and DT_* extents, and advances one physical cursor
through every span, including interleaved framing cells and the snapshot region
band. Focused validation on a private copy of the current engine:
`tools/engine-size-test.f` and `tools/two-generation-test.f` pass;
`tools/engine-size-doc-test.f` reports the expected generated-row drift
(dynamic metadata 200/header pad 3608 after replacing the old last-nonzero
split). The long `tools/hb-build-test.f` run was stopped to avoid competing
with Hazel's serialized full gate and remains pending there.
