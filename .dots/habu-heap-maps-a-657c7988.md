---
title: "HEAP maps a page per record: Habu has no general memory allocator"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T20:14:17.622638+02:00"
---

Problem (Joel 2026-11-11: 'why in the world does heap allocate a page every time?! what kind of wasteful stupidity is this?'; 'ARENA is an allocator to me, a bump allocator whereas HEAP is a memory allocator'): HEAP's NAME-NEW is one mmap per record and NAME-RELEASE one munmap (src/core/dynamic-storage.f RECORD-NEW :214-215, RECORD-RELEASE :220-221; src/core/heap.f header), so a record of any size costs a 16 KiB page and two system calls; added 2026-10-31 with no recorded reason. Cause: nothing sits between OS mappings and a program's object, so HEAP, every arena and every child arena map on their own (dot habu-every-arena-maps-b78ac45c is the same cause). Callers: zen src/document/distill-pdf.f:286, src/document/outline.f:32, src/sql.f:82; tests. Settled (Joel 2026-11-11, parent campaign habu-storage-one-arena-42050605, point 3): the heap is the system allocator on macOS and Linux; no free lists of our own there. Name (Joel 2026-11-12, interview, the chosen option): "HEAP:ALLOCATOR (Recommended)", the public word ( -- ptr ALLOC:allocator ) of a new package HEAP in src/lib/heap.f. Acceptance: HEAP:ALLOCATOR answers an allocator record (src/lib/allocator.f) over libc's calloc, realloc and free; 1000 small records map no page each; a refused request throws E-MEM-MAP and hands out no null block; its test passes on macOS arm64 and a linux-aarch64 build of it runs on Linux. The HEAP NAME t definer and its callers go with habu-replace-the-arena-6bdcc584; Wasm has no heap (parent, point 6). Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-heap.
