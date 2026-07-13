---
title: Bound PTY writes
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T04:49:18.294407+02:00\""
---

Full context: lib/process-pty.f WRITE-ALL uses blocking write and public SEND/SEND-LN have no writable poll or deadline. A child that stops reading can hang gates forever once payload exceeds the PTY buffer. Make the master nonblocking, model partial-write progress, poll writable with a deadline, and add a child-never-reads oversized-send regression. Group with active PTY hardening dots.
