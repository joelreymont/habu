---
title: Decide whether create/allot memory is cell-aligned storage
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T19:34:32.903547+03:00"
---

Problem: lib/json-read.f INIT refuses a create ... allot block with E-STORAGE -3913 (not cell-aligned) on the current root engine while the older dc719a7b engine accepted it; MEM:ALLOC-BYTES memory is accepted by both (tender-b3, 2026-09-11, Tender test moved to the allocator). Either create is meant to align its data space and the engine regressed, or allocator memory is the only aligned source and the typed-storage reader is right to refuse. Acceptance: one documented rule, enforced by the responsible layer, with a rejected-program test for the refused shape and an accepted one for the aligned source; docs/forth.md already states the allocator rule provisionally. Files: src/habu/habu1.f or habu2.f (create/allot data space alignment, rowan-owned) or src/core/layout-buffer.f and lib/json-read.f (the typed storage check). Verify: a reduced script with create 64 allot handed to JSON-READ INIT on bin/hb, then the fixed layer's suite. Depends: none. Ownership: hazel investigates; the engine side goes to rowan if that is the layer. Claim: unassigned.
