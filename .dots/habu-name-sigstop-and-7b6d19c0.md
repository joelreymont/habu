---
title: Name SIGSTOP and SIGCONT in SIGNAL
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T22:27:48.312368+02:00"
---

Problem: src/lib/signal.f:30-45 names SIGHUP, SIGINT, SIGTERM, SIGUSR1 and SIGUSR2 only, so a program that freezes and resumes a child process has no checked name for the stop and continue signals. Maki's release rehearsal freezes a build child to restart the service under it and would spawn /bin/kill for it (reported by tim for Maki). Acceptance: SIGNAL:SIGSTOP and SIGNAL:SIGCONT are public words with an arm per target, as SIGUSR1 is (Linux 19 and 18, macOS 17 and 19; E-PROC-HOST elsewhere); a test stops a spawned child with PROCESS:KILL-RAW and SIGSTOP, shows it stopped, continues it with SIGCONT and sees it finish; docs/forth-card.md or docs/forth.md name the two words where the other signals are named. Files: src/lib/signal.f, its test beside the existing signal test, the doc that lists the signals. Verify: that test through bin/hb --load test/run.f -- <its name>, on macOS; the Linux numbers are checked on a Linux host when one runs the suite. Depends: none. Ownership: unassigned. Claim: agent=bob workspace=.jj-ws.noindex/bob-sigstop.
