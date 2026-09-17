---
title: Let write report its errno instead of a bare -1
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T15:51:17.277053+03:00"
---

Problem: src/habu/habu1.f SYS-PUSH (:1802) publishes a bare -1 for every failed syscall, and the file's own errno rule makes poll the one wrapper that carries -errno, so checked code cannot tell EAGAIN from EPIPE or EINTR on a write. lib/process.f PROC-WRITE-STDIN-ACTIVE therefore infers back pressure (poll said writable with no error bits, write refused) rather than naming it (nonblock lane e7f66a39, 2026-09-17), and its broken-pipe answer rests on poll reporting POLLERR on a readerless write end, measured on Linux only. Acceptance: write (and read, which has the same EAGAIN/EINTR question on a non-blocking descriptor) answer -errno the way poll and kill-errno do, or a sibling word does if changing write's contract breaks callers that test for -1; every caller in lib and tools that compares a write or read result with -1 is swept and keeps its behaviour; PROC-WRITE-STDIN-ACTIVE names EAGAIN as back pressure and closes on EPIPE; regression: a full non-blocking pipe answers EAGAIN, a readerless one EPIPE. Files: src/habu/habu1.f, bootstrap/cg/forth.fs (seed mirror), lib/process.f, lib/process-test.f. Verify: lib/process-test.f; byte fixpoint; tools/bootstrap.sh check; test/run.f. Depends: habu-give-o-nonblock-cff35c7a. Ownership: engine syscall wrappers. Claim: unassigned.
