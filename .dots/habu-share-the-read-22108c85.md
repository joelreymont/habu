---
title: Share the read-whole-or-miss catch of the linker and the build tool
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T07:51:57.890590+02:00"
---

Problem: tools/hb-build-core.f INPUTS-READ? (kernel reuse) and src/habu/linker.f READ-WHOLE each catch the same expected read failures (E-FS-OPEN, E-FS-STAT, E-FS-IO, DYNAMIC-STORAGE:E-MAP) around an open, a sized read and a buffer reserve, and turn them into a miss; one fact lives in two places. Acceptance: one word, in the package that owns sized reads, answers a whole file's bytes or a miss, and both call it. Depends: none. Claim: unassigned.
