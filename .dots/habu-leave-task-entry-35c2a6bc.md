---
title: "Leave task-entry's thread start outside its body"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T00:19:10.976529+01:00"
---

Problem: task-entry's primitive span holds the thread-entry code, whose C-SIGNAL-STACK DIE-LINE, prints the pthread caller's x30, so ENGINE-PRIMS marks task-entry as a body a jump may not enter (DNAME-TAIL clear, lane A of dot 8ce59394, jerry-ktail), though task-entry's own path (ADR, push, B over) reads no return address; one tail site is lost. Acceptance: task-entry carries DNAME-TAIL on arm64, the thread-entry code still prints the caller in its die line, and the fixpoint and the engine tests pass. Depends: none. Claim: unassigned.
