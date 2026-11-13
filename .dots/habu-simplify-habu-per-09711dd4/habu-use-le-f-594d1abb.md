---
title: Use le.f and xref.f readers in AOT files
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.090366+03:00"
closed-at: "2026-11-04T11:48:50.382325+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). Hand-rolled readers in aot-capture.f, aot-closure.f, aot-file.f, link-x64.f go with the window build and three linkers; debug.f:37 residue [i]"
---

Problem: ten hand-rolled little-endian accessors (aot-decl.f:613, aot-closure.f:48, aot-capture.f:51, 54, 76, aot-file.f:258-273, debug.f:37) sit beside src/lib/le.f. Three private copies of the dictionary-record readers (aot-capture.f:58-68, aot-closure.f:96-112, link-x64.f:273-274) duplicate xref.f:84-160, and AOT-WL-RECORD duplicates xref WL-RECORD. Acceptance: these files use le.f and xref.f; the copies are deleted; AOT output unchanged. Files: those named. Verify: aot tests; bin/hb --load test/run.f (capture). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
