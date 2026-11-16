---
title: "Help an engine-made reserve's E-MAP by its owner"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:46:13.043151+01:00"
---

Problem: src/core/dynamic-storage.f MAP-REJECT (:68-76) helps every refused reserve with 'reserve fewer elements, or release storage the program no longer needs', also for reserves the engine makes itself: the stdin stream's TEXT (src/habu/main.f STREAM/STEP) and the interpreter's LOCAL-NAMES. Under ulimit -v, a 300 MB program piped to hb prints that help though the program reserved nothing. Acceptance: that run's E-MAP record helps by what the engine was holding (the program on stdin, and so on for each engine-made reserve), while a program's own reserve keeps today's help; a test fails first. Depends: none. Claim: unassigned.
