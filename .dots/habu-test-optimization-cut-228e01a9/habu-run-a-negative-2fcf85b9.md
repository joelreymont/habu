---
title: Run a negative ?do count zero times
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-30T18:40:09.955428+02:00\""
closed-at: "2026-10-01T12:50:20.136657+02:00"
close-reason: Landed in round-4 integration as f7d0b670 (r4-loop 65d36222); Fable review ACCEPT
---

Problem: `-1 0 ?do … loop` and `-1 0 do … loop` each run their body once with i = 0 (measured on d40cc36d by the r4-neg lane), so docs/forth-card.md's own fill idiom `u 0 ?do 0 a i + c! loop` writes one byte for a negative u, and every counted loop over a caller-supplied count does one step of work on an invalid count. The card documents only `0 0 do` (once) and `0 0 ?do` (zero times). Neither ANS (?do skips only on equality and wraps) nor a skip rule is what runs. `?do … n +loop` with a negative step legitimately has limit below start, so a plain signed skip in ?do is wrong for +loop. Acceptance: the semantics of ?do with limit below start is decided for loop and for +loop with either step sign (design first), stated in docs/forth-card.md and docs/forth.md, and implemented identically at every tier (interpreted, tier 0, native) and in the checker's loop model; a census of ?do sites whose count can be negative finds none that relies on the old one-step behaviour; tests through the real load path cover -1, the minimum cell and 0 for loop, and a counting-down +loop, seen to fail first. Files: the loop words in src/core and src/compiler, the checker loop model, docs. Verify: loop suites, native build, two-generation chain, full native suite. Depends: none. Ownership: counted-loop entry semantics. Claim: unassigned.
