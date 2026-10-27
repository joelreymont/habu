---
title: Close the pipe pair when CLOEXEC fails
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T20:42:53.778271+02:00"
---

src/lib/process.f PIPE-PAIR-CLOEXEC-HELD (about :305-309) opens a pipe, then FD-CLOEXEC! on each end; a throw from either fcntl leaks both descriptors. Fix: close both ends on any throw before rethrowing (the pair is not yet handed out). Test: a rejected CLOEXEC is hard to provoke; if no real failure path can be driven, the fix is read-reviewed. Reported by phil from a Zen review (Zen master f90a9c32 uses PIPE-PAIR-CLOEXEC for its worker stop pipe).
