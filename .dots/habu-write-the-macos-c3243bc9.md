---
title: Write the macOS wait queue and AIO host layer
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T20:00:21.118879+01:00"
---

Problem: the kept-alive design (per-task queues; Linux epoll, macOS kqueue) is built for Linux only, on Joel's 'no macos anymore. we'll fix it later': on macOS TASK:QUEUE, TASK:QUEUE-WAKE and src/lib/aio/macos.f (Q-ARM, Q-DISARM, Q-WAIT, EV-ID, EV-REVENTS over kqueue, EVFILT_USER with EV_CLEAR for the wake) do not exist. Q-ARM takes the queue it arms ( q fd events id -- rc ): AIO:POLL-INTO arms the holder's queue from another thread. Acceptance: on macOS a kept-alive HTTP request costs two thread switches, as on Linux, and the AIO and task tests and test/file-objects.f pass there; kqueue's readiness for a pty master and a serial device is confirmed first. Depends: the Linux lanes (TASK queue, AIO onto per-task queues, HTTP hand-back). Claim: unassigned.
