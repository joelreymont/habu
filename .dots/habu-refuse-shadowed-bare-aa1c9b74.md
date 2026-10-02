---
title: Refuse shadowed bare words at the interpreter
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T14:24:42.654969+03:00"
closed-at: "2026-10-01T14:42:23.930908+03:00"
close-reason: "Engine leaf LFINDSHADOW and OUTER SEARCH refuse 105; using-test and outer-interpret TOP-SHADOW fail on the base engine and pass on product c3905243 (gen1 = gen2); full gate 515 of 516 green, its one red (outer-find) adapted and green."
---

Problem: docs/forth.md (the E-USING-SHADOW-GLOBAL rule) says a bare tail that resolves to a global while a used package also exports it is refused at the reference site, but only checked definitions enforce it (checker 7141, rc 67). At top level and for ' the engine's lookup (src/habu/habu2.f EM-INTERPRET-FIND, C-TICK) and the Habu loop's OUTER SEARCH bind the global silently; the only top-level refusal came from the tier-1 top-row tracker's effect query, which tier 1 must not do (habu-keep-the-top-086a67f0). Measured: using PS then a top-level SHW prints the global's value. habu-catch-bare-x86-8b1b5fb4 needs this: its guard is using X64LAYOUT over each x86 source.
Acceptance: when FIND's hit is a global (wordlist 0) and a live used public wordlist also exports the token, the interpreter and ' refuse with a new ENGINE-ERROR:USING-SHADOW-GLOBAL (105) naming the token, delivered the way the engine's top-level E-UNDEFINED is (catchable under catch and evaluate, an exit at top level). Unchanged: a qualified PKG:TOK, a word the open package defines itself, a token no used package exports, and checked definitions (rc 67). The Habu loop (src/habu/outer.f SEARCH through FIND-USED) agrees with the engine. Tests written first and failing on the base engine: test/using-test.f (top-level read, tick, qualified, unshadowed, own package, a buffer evaluated under the using, after ;using) and a TOP-SHADOW case in test/outer-interpret.f through both loops. Any tree source the full suite shows reading a shadowed global at top level is qualified in this commit. Docs: docs/forth.md rule text (105 at top level, 67 in a definition; a file loaded under a using resolves through it) and the forth-card row.
Files: src/core/engine-error.f, src/habu/habu1.f, src/habu/habu2.f (a leaf beside EMIT-FIND-USED, called on the LFIND-hit paths of C-TICK and EM-INTERPRET-FIND), src/habu/outer.f, test/using-test.f, test/outer-interpret.f, docs/forth.md, docs/forth-card.md. bootstrap/cg/forth.fs needs no mirror: the recovery engine may bind the global as today.
Verify: spark rebuild from the base product and gen2; the two suites on the product; clobber-lint; the lead runs the chain, the full gate and Gforth recovery.
Depends: none.
Ownership: krait (Intel lane).
Claim: unassigned.
