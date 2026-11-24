---
title: Give allocations a typed span, not ^u8 n
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.115709+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). src/lib/memory.f MEM-ALLOC-BYTES, ALLOC-BYTES, RELEASE-BYTES, ALLOCATION>SPAN, WITH-BYTES, ALLOC-GUARDED, RELEASE-GUARDED; allocator.f BYTES, RESIZE; arena.f ALLOC, ALLOC-DISPOSE; c2-memory.f WITH-MUT, WITH-READ, WITH-MUT-LOAN, WITH-INIT, WITH-RECORDS, WITH-RECORD, WITH-FIELD; browser/event.f FREE, src/lib/zip-state.f BYTES-FREE hand out or take a block as ^u8 n (see also habu-make-heap-wasm-15e5b233). Acceptance: a design names the block's type (SPAN:span<u8> already exists) and every allocator and caller states it; no block travels as ^u8 n. Depends: none. Claim: unassigned.
