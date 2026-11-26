---
title: Lose no signal a task-thread stub holds at RELEASE
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T12:56:26.532165+01:00"
---

Problem: src/lib/signal.f RELEASE restores the caught set, disarms, drains and closes the pipe, but a caught signal delivered to a task thread runs SIGNAL's stub there; a stub between reading the fd word and its write() when RELEASE runs on the owner's thread loses the signal (neither the pipe nor the default action gets it) or writes into a descriptor number reused after the close. Single-threaded programs are unaffected. Acceptance: with tasks running, every caught signal delivered before RELEASE returns reaches DRAIN's result or its default action, and no stub writes after the close; a test that fails today shows it. Depends: none.
