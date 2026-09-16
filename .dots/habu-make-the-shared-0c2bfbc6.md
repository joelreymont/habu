---
title: Make the shared library builders task-local
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:58:32.421614+03:00"
---

Problem: lib/string.f's SB builder, lib/fmt.f's number buffer and lib/fs.f's open descriptor, length and path slots (FS-IO-FD and friends) are single process-wide cells, so two tasks using SB, FMT or READ-ALL/FILE-SIZE/the FS-* predicates interleave or corrupt each other; lib/net/tcp4.f made the opposite choice (task-local endpoint and poll storage through TASK:+USER) and Tender's HTTP server has to hold a TASK:FACILITY across every use and read its static tree before any task starts (aspen, 2026-09-17, /tmp/aspen-task-safe-library-state.md). Nothing tells a caller which library is single-task without reading its source. Acceptance: SB, the FMT number buffer and lib/fs.f's per-call slots live in task-local storage the way tcp4.f's do (TASK:+USER rows), so a task's builder is its own; each module header and docs/threads.md state per library whether its state is process-wide, task-local or caller-owned, and a test in lib/task-test.f (or the module's test) runs two tasks through SB and FMT concurrently and checks neither sees the other's bytes; engine size before and after on a native-runtime engine since string.f, fmt.f and memory.f are baked. Files: lib/string.f, lib/fmt.f, lib/fs.f, docs/threads.md, docs/stdlib.md, lib/*-test.f. Verify: lib/string-test.f, lib/fmt-test.f, lib/fs tests, lib/task-test.f; fixpoint; test/run.f. Depends: none. Ownership: stdlib task discipline. Claim: unassigned.
