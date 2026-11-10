---
title: Linux holds SIGPIPE off for the whole process
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T10:28:37.050917+02:00"
---

Problem: FD:FD-NOSIGPIPE! sets a flag on one open file description on macOS (fcntl 73). Linux has no such command, so both Linux kernels emulate it by ignoring SIGPIPE for the whole process (src/habu/kernel-x64.f:1432-1444, src/habu/habu1.f:1020-1042) and nothing can put it back. After the first such call a Linux program gets E-FS-IO from every write to a broken pipe where the same program on macOS still dies of SIGPIPE: one machine behaves differently from another. Found while designing habu-a-log-line-839239b8, 2026-11-10. Acceptance: holding SIGPIPE off for one descriptor or one call means the same on every target, and can be undone on every target. The design is the lead's and changes how signals are handled, so it goes to Joel before a worker starts. Files: the Linux kernels' fcntl, src/lib/fd.f. Verify: a test that a write to a second broken pipe still raises SIGPIPE after the first descriptor was armed, on each target. Depends: habu-a-log-line-839239b8 (adds FD:WITH-NOSIGPIPE). Ownership: the kernels' signal handling. Claim: unassigned.
