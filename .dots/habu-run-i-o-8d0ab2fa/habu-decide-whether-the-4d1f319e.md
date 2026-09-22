---
title: Decide whether the AIO loop task forbids compilation
status: closed
priority: 3
issue-type: task
created-at: "2026-09-21T18:07:21.892693+03:00"
closed-at: "2026-09-22T04:28:46.976413+03:00"
close-reason: "Measured: the ban protects the code band's W^X flip, aligned outward to the 64K PROT-PAGE-MAX unit - lib/aio.f loaded last, loop + one client task ticking 1 ms timeouts, live count reset by hand, 2000 empty definitions: SIGSEGV at pc 14.7K below CP inside lib/aio.f; with 32K of definitions between lib/aio.f and the loop start (CP in the next unit) the run survives 21 client rounds. No exemption: it would need every live task's code out of CP's unit, which no later definition can promise, and forget paths free code under a running task. docs/threads.md, docs/aio.md and docs/genio.md state the reason; define first, start the loop last."
---

Problem: docs/threads.md: compilation and dictionary mutation are forbidden while any task is live (TASKS-LIVE-CELL, exit $4F). The AIO loop is a task, so a program that starts the loop can no longer define a word - a REPL served over a connection (genio.md) stops accepting definitions once its own transport waits through the loop. Acceptance: measure what the ban protects against for a task that executes only words compiled before LOOP-START and never touches the dictionary (code-region growth, record reallocation, the JIT), then either exempt the loop task from the count with a regression that compiles while the loop runs, or state in docs/aio.md and docs/threads.md why the ban stays and what a REPL does instead (define first, start the loop last). Files: lib/task.f or src/habu (the live count), docs. Verify: test/run.f. Depends: habu-add-the-io-0c5a9630. Ownership: hazel. Claim: unassigned.
