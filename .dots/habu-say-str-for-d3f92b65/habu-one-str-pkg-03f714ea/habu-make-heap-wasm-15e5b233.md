---
title: "Make heap/wasm.f RESIZE-BYTES's allocator slot take a vector or str"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:08:17.819219+01:00"
---

Problem: src/lib/heap/wasm.f RESIZE-BYTES ( ^u8 ^u8 n n -- ^u8 ) moves a block, and its row is fixed by the shared ALLOC:allocator resize slot (heap.f RESIZE-BYTES, allocator-test P-RESIZE). Acceptance: the slot's row says what it moves without ^u8 n, every allocator implements it, and heap/wasm.f passes check.f and its suites. Depends: none. Claim: unassigned.
