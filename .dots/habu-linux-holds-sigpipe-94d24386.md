---
title: Linux holds SIGPIPE off for the whole process
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T10:28:37.050917+02:00"
---

Problem: FD:FD-NOSIGPIPE! sets a flag on one open file description on macOS (fcntl 73). Linux has no such command, so both Linux kernels emulate it by ignoring SIGPIPE for the whole process (src/habu/kernel-x64.f:1432-1444, src/habu/habu1.f:1020-1042) and nothing can put it back. After the first such call a Linux program gets E-FS-IO from every write to a broken pipe where the same program on macOS still dies of SIGPIPE: one machine behaves differently from another. Found while designing habu-a-log-line-839239b8, 2026-11-10. Acceptance: holding SIGPIPE off for one descriptor or one call means the same on every target, and can be undone on every target. The design is the lead's and changes how signals are handled, so it goes to Joel before a worker starts. Files: the Linux kernels' fcntl, src/lib/fd.f. Verify: a test that a write to a second broken pipe still raises SIGPIPE after the first descriptor was armed, on each target. Depends: habu-a-log-line-839239b8 (adds FD:WITH-NOSIGPIPE). Ownership: the kernels' signal handling. Claim: agent=bob workspace=.jj-ws.noindex/habu-linux-holds-sigpipe-94d24386

Design inputs added 2026-11-10 (bob), from the census (/Users/joel/Work/reviews/habu-10x-2026-11-10/session2-notes.md "SIGPIPE census"): (1) Linux's spawn path never resets SIG_IGN before execve, so every program a Linux process starts after its first FD-NOSIGPIPE! inherits SIGPIPE ignored. (2) test/x86-64-kernel-syscalls.f:347 pins fcntl 73 with arg 0 as still ignoring SIGPIPE; on macOS arg 0 clears the flag. (3) The wake pipes at src/lib/curl.f:1063 and src/lib/ws.f:290 and the signal stub's self-pipe are never armed. (4) io_uring OP_WRITE gets no SIGPIPE handling; whether it raises is not determined. (5) No target has a word for the thread signal mask, pending signals or waiting for one. Not measured: whether macOS delivers a write's SIGPIPE to the writing thread.
