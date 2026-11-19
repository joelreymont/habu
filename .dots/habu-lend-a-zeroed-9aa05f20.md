---
title: Lend a zeroed typed record for one quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:01:17.697144+01:00"
---

Problem (stuart, Tender, 2026-11-19): Tender has at least 118 sites that open X-BYTES [: ... ;] MEM:WITH-BYTES for one record and cast it (tender src/admit.f:443 CAST: >PLAN ( ^u8 -- ^plan )), so each quotation's stack spells ^u8 NUM:alloc-byte-len, which Joel's rule (2026-11-19, "no agent should be using "^u8 n" anymore! it's either str or vector") rules out; ARENA:WITH costs 3.3 us per open against 0.1 us for WITH-BYTES. Acceptance: a MEM word ( R width<t> [ R ^t -- S ] -- S ), called as [type] t [: ... ;], lends a zeroed t at WITH-BYTES's cost and frees it on exit and on a throw; a test shows both; Tender's dot Tender-hold-working-storage-4c261067 can drop its casts. The word's name is Joel's (Tender calls it). Depends: none. Claim: agent=jerry workspace=none yet.
