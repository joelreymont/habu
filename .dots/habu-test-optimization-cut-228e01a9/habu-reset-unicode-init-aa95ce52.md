---
title: "Reset unicode INIT's claim in a forked child"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.048377+03:00"
---

Found by lane 449 forklocks: lib/unicode.f:98-110 INIT claims READY 0->1 by CAS and spins while READY is 1. A child forked while another task holds READY at 1 (LOAD-SYMBOLS in flight) spins forever: no task in the child will ever store 2 or 0. Fix through the FORK-CHILD registry (lib/process-fork.f, lane 449): in the child, a READY of 1 drops the half-loaded symbols and returns to 0. Acceptance: an E2E hammer (one task looping INIT on a fresh package state while the main task forks children that call a unicode word) hangs some children before the fix and none after; bounded by a deadline.
