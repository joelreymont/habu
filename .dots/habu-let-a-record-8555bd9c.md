---
title: "Let a record's own package view its bytes"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T15:04:35.442732+02:00"
---

Problem (arena 2c worker, carl-arena3 on 70ed7d74): a structure pointer does not fit 'ptr a' (docs/forth.md 'A callback's pass-through context is a, not ptr a'), so BYTE-VIEW and munmap ( ptr a n -- n ) refuse 'ptr node' (E-MISMATCH expected 'ptr a' actual 'ptr node'). A record laid at the base of mapped bytes must keep a second copy of its own address as bytes to free them: src/lib/arena.f's arena keeps 'self ptr u8', and chunk's links stay 'ptr u8' with casts back (>CHUNK) because NEWEST and UNMAP-CHUNK need the chunk's bytes. A byte view of any record would expose an OPAQUE record's layout outside its package. Acceptance: a design, routed by the parallel-agents rule, that says whether and where a record pointer may be viewed as bytes (its own package only, or not at all) and, if it may, arena.f's self field and chunk casts go. Files: src/core/checker.f (ptr unification), docs/forth.md, src/lib/arena.f. Verify: the design's own cases. Depends: none. Ownership: the ptr a rule. Claim: unassigned.
