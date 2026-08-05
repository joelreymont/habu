---
title: Reach the four unpinnable C-CALL clauses
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T21:45:16.232254+02:00"
---

tools/codegen-workload-test.f pins five of C-CALL-REJECT-UNSAFE's nine refusal clauses per clause (bl, b, b.cond, cbz, blr): each has a fixture under INL-MAX whose only unmovable instruction is of that class, cross-checked against the engine through a caller's emitted call. Four clauses have no such fixture and no real path can produce one in this process: tbz/tbnz (src/arch/arm64/asm.f has no encoder at all; the only mask matches in the live dictionary are inline string DATA read as instructions, inside records far past the limit), br (hand-written engine assembly only - evaluate at 228 bytes, throw at 284, both carrying several other refused instructions), a ret inside a body (only where a quotation body is compiled inline, which also emits the b that jumps over it; smallest such body is 52 bytes), and adr (only for an inline string literal, which also emits the b over the literal's bytes). A body carrying two refused instructions isolates neither. So deleting any of those four clauses from the engine or from the scan's copy still passes the suite. Fix needs a real path that emits one of the four into an otherwise movable under-40-byte body - a checked construct that compiles to a tbz, an adr without its skip branch, or a published routine built through an audited boundary whose bytes the suite asserts. Not a hand-assembled record fed around the compile path: that would pin the copy against itself and prove nothing about the engine. Evidence and reasoning are recorded in CLAUSE-CASES in tools/codegen-workload-test.f.
