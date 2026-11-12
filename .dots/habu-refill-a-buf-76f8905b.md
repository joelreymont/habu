---
title: Refill a buffer in place
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T16:50:27.655786+02:00"
---

Problem: a caller that refills one BUF header per item has only BUF:DISPOSE then BUF:INIT, an munmap and an anonymous mmap per fill (src/lib/memory.f), or a word of its own that picks BUF:CLEAR for a live header and BUF:INIT for a dead one, since INIT throws on a live header and CLEAR on a dead one. Maki has that word twice (FRESH in src/build.f and src/step-source.f) and 23 dispose-then-init sites, two syscalls per row in STEP import (request from Maki, tim, 2026-11-12; /tmp/tim-habu-buf-refill.md). Acceptance: one public BUF word ( ptr a NUM:byte-len -- ) empties a live buffer, keeping its mapping, or starts a dead one at the given capacity. The word's name is Joel's. Depends: none. Claim: unassigned.
