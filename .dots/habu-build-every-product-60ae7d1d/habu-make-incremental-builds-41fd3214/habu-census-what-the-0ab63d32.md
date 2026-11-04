---
title: Census what the incremental build refuses
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T11:49:22.954806+02:00"
closed-at: "2026-10-29T12:17:44.668907+02:00"
close-reason: "Measured: 8 of 192 files get objects; judging yields 0"
---

Joel 2026-10-29 asked why a file is ever refused: an object newer than its source is used, else the source compiles. Measure on the engine build (worker, workspace carl-census, brief .out/brief-census.md): files cached vs refused by reason, files whose object depends on judged top-level runs and the seed words they run, warm and memory.f comment-edit rebuilds. The numbers decide whether judging and refusal machinery is cut to: no object for a file whose top level runs anything but definitions. carl-abandon and the OBJFILE rename wait on it.

Result (census report carl-census/.out/census-report.md): engine build opens 192 files, objects for 8; 147 of 155 engine-source files refused (ordered-dependency 69, mostly mapped-compile-time-read; earlier-write 42; run-reaches-kernel-word 13; earlier-write-before-load 8), 37 later files refused tier. Judging admitted 80 runs in 25 files, none in an object file: judging yields 0 objects. Cold 168 s user, warm no-edit 147 s (8 reused), memory.f comment edit the same. Next: Fable redesign of the incremental build.
