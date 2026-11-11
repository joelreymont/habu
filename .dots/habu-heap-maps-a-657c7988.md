---
title: "HEAP maps a page per record: Habu has no general memory allocator"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:14:17.622638+02:00"
---

Problem (Joel 2026-11-11: 'why in the world does heap allocate a page every time?! what kind of wasteful stupidity is this?'; 'ARENA is an allocator to me, a bump allocator whereas HEAP is a memory allocator'): HEAP's NAME-NEW is one mmap per record and NAME-RELEASE one munmap (src/core/dynamic-storage.f RECORD-NEW :214-215, RECORD-RELEASE :220-221; src/core/heap.f header), so a record of any size costs a 16 KiB page and two system calls; added 2026-10-31 with no recorded reason. Cause: nothing sits between OS mappings and a program's object, so HEAP, every arena and every child arena map on their own (dot habu-every-arena-maps-b78ac45c is the same cause). Callers: zen src/document/distill-pdf.f:286, src/document/outline.f:32, src/sql.f:82; tests. Acceptance: a general allocator (free lists over mapped chunks, one piece freed at a time) under HEAP, a record costing about its size; arenas able to draw chunks from it; designed first (threads, failure recovery: the design goes to Joel before a worker). Files: src/core/dynamic-storage.f, src/core/heap.f, src/lib/arena.f. Verify: 1000 small records map no more than the bytes they hold plus bounded slack. Depends: the storage shape under design in dot habu-give-an-arena-25a7d3cc. Ownership: carl. Claim: unassigned.
