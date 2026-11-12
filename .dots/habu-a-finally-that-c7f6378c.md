---
title: "A finally that rethrows the body's code first"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:39:19.999405+02:00"
---

Problem: finally lets the cleanup's throw replace the body's. Maki needs the body's code first and writes it by hand with two catches in four places (src/kernel.f KERNEL:WITH, src/session.f RUN-IN, src/storage.f WITH-SCRATCH, session END/CLOSE); the hand-written form skips the cleanup on TASK:HALT, which finally runs (request from Maki, tim, 2026-11-12; /tmp/tim-habu-finally-first.md). Acceptance: one public word beside finally, with finally's halt behaviour, runs the cleanup on every path and rethrows the body's code first, else the cleanup's; a test holds each path. The word's name is Joel's. Depends: none. Claim: unassigned.
