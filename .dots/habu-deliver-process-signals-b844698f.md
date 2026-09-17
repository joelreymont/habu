---
title: Deliver process signals to a Habu program
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T08:35:23.564344+03:00"
---

Problem: Habu has no signal facility, so a server cannot act on SIGTERM or SIGINT: the Tender process entry stops through a stop file polled once a second (server/main.f, 2026-09-17) because a handler would have to enter Habu code from a C signal context. Acceptance: package SIGNAL delivers a chosen set of signals to the program safely: a tiny C-ABI trampoline (the same entry shape task-entry uses) installed through sigaction via FUNCTION: writes the signal number to a self-pipe, and SIGNAL:WAIT ( ms -- signal-result ) or a readable descriptor the program can poll beside its sockets answers it in ordinary checked code; SIGTERM and SIGINT at least; a test sends the process its own signal through kill(2) and observes it; restart on EINTR documented for the blocking words that need it. Files: lib/signal.f, its test, docs/threads.md or docs/signal.md, src/habu if the trampoline needs an engine entry. Verify: the test; test/run.f. Depends: none. Ownership: lib/signal.f. Claim: unassigned.
