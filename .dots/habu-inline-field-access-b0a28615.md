---
title: Inline constant-offset field access in compiled code
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:10:16.952407+02:00"
---

Problem: tier 0 compiles a call of a FIELD child (body kind offset, src/compiler/fixed-body.f) as a call of its fixed body, which adds the offset and returns (src/habu/t0.f WORD-CALL :1671 and the offset arm of FIXED-BODY :2085), and an index child the same way, so every record field access costs a call and a return. A JSON:VALUE= profile (46,782 samples, on the Mac before field-project's deletion) put 14.5% of samples in the field projection and about 9% in the reader's CURSOR-* field words; one plain read of a 290-byte row cost 3.9 us. The store guard's share is habu-cut-the-store-50159b7b.
Acceptance: tier 0 compiles a call of an offset child as an add of its offset, and of an index child as its multiply-add, with no call; a JSON read profiled before and after shows the field calls gone and the per-row time down by their share.
Depends: none. Claim: unassigned.
