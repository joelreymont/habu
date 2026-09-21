---
title: Decide whether the AIO loop task forbids compilation
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T18:07:21.892693+03:00"
---

Problem: docs/threads.md: compilation and dictionary mutation are forbidden while any task is live (TASKS-LIVE-CELL, exit $4F). The AIO loop is a task, so a program that starts the loop can no longer define a word - a REPL served over a connection (genio.md) stops accepting definitions once its own transport waits through the loop. Acceptance: measure what the ban protects against for a task that executes only words compiled before LOOP-START and never touches the dictionary (code-region growth, record reallocation, the JIT), then either exempt the loop task from the count with a regression that compiles while the loop runs, or state in docs/aio.md and docs/threads.md why the ban stays and what a REPL does instead (define first, start the loop last). Files: lib/task.f or src/habu (the live count), docs. Verify: test/run.f. Depends: habu-add-the-io-0c5a9630. Ownership: hazel. Claim: unassigned.
