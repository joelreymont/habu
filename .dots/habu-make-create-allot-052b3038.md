---
title: Make create allot zero its space
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:56:40.988818+02:00"
---

docs/forth.md:1864 promises that create … allot, BUFFER: and TYPED-BUFFER allot zeroed space. On master bin/hb: '-1 here !  -1 here cell+ !  create P 3 cells allot  P @ .' prints -1, so stale bytes past HERE survive into the new block. src/lib/source-lex.f:72-86 and tools/lint/intern.f:28 allot headers without zeroing and rely on the promise. Find where allot/BUFFER: lay down space, decide whether allot itself zeroes (the doc's contract) and fix that layer; add an E2E row that dirties space past HERE, then allots and reads zero. Found by the Etch VEC review (CLEAR-HEADER kept for this reason).
