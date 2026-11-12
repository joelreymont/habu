---
title: "Refuse the target words a cross-build runs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:45:05.542034+02:00"
---

Problem: a target word a cross-build runs is refused by name (docs/xcomp.md "Host and target words") only once its source puts it in a target section. Source: Joel, 2026-11-12: 'the target must never require anything to run on the host'; asked whether Habu's sources get host marks: 'the default section is always host. there should be no need to change anything in habu'. Acceptance: a program with a CALLBACK: builds for linux-aarch64 and its callback runs there; the words that write through MAIN-BASE (src/lib/ffi-callback.f ENTRY, src/lib/task.f CONTEXT-BIND, CALLER-BIND, TASK-LIVE+ and TASK-LIVE-) sit in a target section, so the subject src/lib/ffi-callback-test.f WRITE-SUBJECT writes, whose top-level line `FILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop` kills a linux-aarch64 build by SIGBUS, is refused E-TARGET-WORD; a native build refuses a target word it runs too, after a count of the words Zen, Tender and Maki's image call at top level. Depends: none. Claim: unassigned.
