---
title: "Type an allocator's state and counts"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:20:51.449146+01:00"
---

Problem: src/lib/allocator.f:17-20 types an allocator's functions [ ^u8 n -- ^u8 ], [ ^u8 ^u8 n n -- ^u8 ] and [ ^u8 ^u8 -- ]: a state pointer, then a block and byte counts. Under the card rule that a row never says ^u8 n, every implementation spells them ( str -- ^u8 ) and ( ^u8 str n -- ^u8 ), a str that is no string: src/lib/vector-test.f:148-163 (P-ALLOC, P-RESIZE), src/lib/json-write-test.f (JWT-MOVE-*), Tender test/tenderd/secrets-test.f:71. Acceptance: allocator.f's rows give the state and the counts their own types; heap.f, arena.f and every test allocator state them; no allocator row or implementation says str or ^u8 n for them; the allocator, vector and json-write suites pass; Tender's test is told the new rows. Depends: none. Claim: unassigned.
