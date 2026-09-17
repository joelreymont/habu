---
title: Name the errno of a refused read in checked code
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:20:32.150117+03:00"
---

Problem: the engine's syscall wrappers collapse every failed syscall but poll to a bare -1 (src/habu/habu1.f, 'THE ERRNO RULE FOR THIS FILE'S SYSCALL WRAPPERS'), so checked code cannot tell EAGAIN from EINTR from EBADF on read: src/habu/repl.f KEY1 treats any read below 1 as end of input (habu-make-key1-end-cabdb980), and lib/signal.f SIGNO@ concludes EAGAIN structurally from owning a non-blocking descriptor whose write end it holds open (aspen 2026-09-17), each with a paragraph explaining why the inference is sound; FFI:ERRNO beside a raw syscall reads a stale libc errno. Acceptance: one engine way to name the errno of a refused read or write (a wrapper variant that answers the negated errno, or an errno cell the wrappers fill, chosen against the errno rule's own reasons), declared in the checker, with the two consumers converted to branch on the name and their inference prose removed, the errno rule text updated, and rejected or edge cases: EAGAIN on an empty non-blocking pipe, EBADF on a closed descriptor, EINTR under a handler installed without SA_RESTART. Files: src/habu/habu1.f, src/core/checker.f (row), src/habu/repl.f, lib/signal.f, docs/. Verify: lib/signal-test.f; test/proc-pty.f; byte fixpoint; test/run.f. Depends: none. Ownership: engine syscall wrappers. Claim: unassigned.
