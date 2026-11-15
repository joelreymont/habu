---
title: Give fault records their help
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.726955+01:00"
---

Problem: crash.f:196-440 writes the prose and JSON record forms again in machine code; a memory fault has no help (memory-fault.f:18-22 pins that); a stack overflow prints only `= at:` and exits 102. Acceptance: each fault prints one record with a help that says how to fix it; one writer of the record form, or the reason a fault cannot use REJECTION stated beside crash.f. Depends: none. Claim: unassigned.
