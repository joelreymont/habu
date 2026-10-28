---
title: Park a task until woken or a deadline
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T13:01:01.299137+02:00"
---

Problem: a task can park until woken (TASK:STOP, src/lib/task.f:2045) or sleep until a time (TASK:SLEEP), but not both, so a long poll has to sleep the full interval even when its event arrives early. semaphore_timedwait is bound only as a zero-timeout try (MACH-SEM-TRY, src/lib/task.f:290). Caller: Maki src/chat.f GET (:708-720) sleeps 250 ms once whenever a turn has not moved past the client's sequence. Measured 2026-10-28 in Maki test/chat-case.f at load about 20: an A/B build with a 10 ms wait cut the run from 51.1 s to 49.9 s and from 63.2 s to 59.6 s. Turns that answer at once dropped from 260 ms to about 60 ms per poll (PROVIDER-REFUSALS 1.6 s to 0.3 s). The wait costs about 125 ms per poll. A shorter sleep would still be polling. Acceptance: one task word parks the running task until TASK:WAKE or a deadline (ms), whichever comes first, and answers which one happened; it observes TASK:HALT the way TASK:STOP does and works on the main thread's park. Regressions: a wake before the deadline returns early and reports woken; no wake reports the deadline at or after it; a wake that lands as the deadline passes is not lost for the next park (PARK-DRAIN, :652). Joel approved the new public word (interview 2026-10-28). Files: src/lib/task.f, src/lib/task-test.f (macOS Mach semaphore park; any other target park the file grows). Verify: src/lib/task-test.f; test/run.f. Depends: none. Ownership: task runtime. Claim: unassigned.
