---
title: Refresh docs/engine-size.md to the varint-row engine
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T15:43:50.598179+03:00\""
---

Problem: docs/engine-size.md describes a 5,374,144-byte engine with 84,894 eight-byte (offset u32, length u32) DATA run rows and 8,021 package-private records; every number belongs to an engine several landings old, and its format paragraph is wrong twice over since 3afce20f made a row two LEB128 varints (gap from the previous run's end, length) with RUN-GAP-MIN 2: the engine is 3,932,352 bytes, aot/data-run-rows 490,676, aot/data-run-bytes 737,376, 245,194 runs. Dots habu-declare-the-surface-89e9aed0 and habu-ship-only-the-d7d38629 cite the document's figures. Acceptance: every number in docs/engine-size.md is re-measured with bin/hb --load tools/engine-size.f -- bin/hb on the integrated engine, the row-format paragraph describes the varint row and why the threshold is 2, the 'what would make it smaller' section reflects that run bytes (content) are now the larger term than rows (framing), and the two citing dots' figures are corrected in the same batch. Files: docs/engine-size.md, .dots/habu-declare-the-surface-89e9aed0.md, .dots/habu-ship-only-the-d7d38629.md. Verify: the tool's output matches the document line for line. Depends: habu-merge-captured-data-c4538778 integrated. Ownership: docs. Claim: agent=hazel-size-doc workspace=.jj-ws/hazel-size-doc.
