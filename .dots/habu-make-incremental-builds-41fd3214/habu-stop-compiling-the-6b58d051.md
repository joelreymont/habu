---
title: Stop compiling the image writer per build
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T14:59:32.118982+03:00"
---

Problem: every product build compiles tools/native-emit.f from source to write the image (READ-SOURCE-WRITER / LOAD-SOURCE-WRITER, tools/native-build-core.f ~:962-1015): 0.77 s of a 6.8 s warm engine build. Evidence: docs/compiler-measurements.md section 11. Acceptance: the writer is part of the support image (or otherwise compiled once per support image) and no product build parses native-emit.f; warm engine build wall drops by about that time; products byte-identical. Files: tools/native-build-core.f, tools/native-build-image.f. Verify: warm engine build timing before/after, two-generation, test/native-file-auto-e2e.f. Depends: none. Claim: unassigned.
