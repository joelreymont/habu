---
title: test/serial-xmodem.py concurrent throws E-MEM-UNMAP
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T04:19:08.517073+03:00"
---

Problem: the concurrent case (two tasks, each an XMODEM session over its own pty) ends with hb: uncaught throw code -3203 from the MAIN task, on the tip before the AIO loop (poll(2) path, /tmp/hazel-poll-2/concurrent-tip-tree.log) and after it alike; the case had never run because the checker refused its own WAIT-DONE ( ptr a -- ) (now ( ptr n -- )). Main-task steps: ACTIVATE x2, WAIT-DONE x2, TASK:KILL x2, DONE atomic@ ., (LOOP-START/STOP). Throwers: lib/memory.f RELEASE-BYTES (:108) and MEM-RELEASE-GUARDED (:135, TASK-RELEASE-MEM under KILL). First step: catch each main-task step and print which throws; then whether a task that ended inside SERIAL-XMODEM leaves an extent KILL cannot unmap. Acceptance: python3 test/serial-xmodem.py exits 0 with the concurrent case. Ownership: hazel.
