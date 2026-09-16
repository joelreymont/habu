---
title: Decide the cooperative task kernel for targets
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:06.967352+03:00"
---

Problem: lib/task.f is pthread-only, so programs for microcontrollers without an OS have no tasks, and the plan needs one task vocabulary on the server and on targets. polyFORTH and SwiftX run a round robin of about thirteen words where each task's STATUS cell is both the scheduler link and the wake flag (PAUSE, STOP, WAIT, WAKE, ACTIVATE, GET/RELEASE); VFX keeps that API on pthreads so embedded and hosted source is shared (docs/tasking-models.md sections 2 and 3). Acceptance: a decision recorded in docs/tasking-models.md fixing the shared public surface of TASK (which words exist on both kernels and which are hosted-only), the STATUS/WAKE representation for the arm32 and tic6x targets, how blocking I/O words yield, and the test strategy on the serial or emulator hosts under test/; it opens the implementation dots. Files: docs/tasking-models.md, docs/threads.md. Verify: the doc answers every point and the child dots exist. Depends: none. Ownership: the two docs. Claim: unassigned.
