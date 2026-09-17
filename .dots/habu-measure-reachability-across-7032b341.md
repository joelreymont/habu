---
title: Measure reachability across the stripped code spans
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:39:26.149411+03:00"
---

Problem: the line-head engine's aot/code-blob is 1743976 bytes but dictionary records own only 696852 of them; the other 1.05 MB is code of stripped words, which tools/engine-size.f scans as ROOT regions (its header says so), so its reachability report (reachable 501080 bytes from the engine-entry roots, 195772 unreachable) says nothing about the largest part of the code and no dead-code decision can be made. The span table (habu-record-the-code-0db56c19, landed 2026-09-17) now names every stripped span. Acceptance: engine-size treats each span as a node with its own B/BL edges, reports reachable and unreachable spans and bytes from the engine-entry roots and from the public dictionary surface, and names the largest unreachable spans by their sidecar names when a .names sidecar sits next to the image; the numbers are recorded in docs/compiler-measurements.md; a follow-up dot for elimination is filed only if the unreachable total is worth it. Files: tools/engine-size.f, src/habu/aot-closure.f (span reader), docs/compiler-measurements.md. Verify: the report on the line-head engine and its sidecar. Depends: none. Ownership: engine size. Claim: unassigned.
