---
title: Derive the vocabulary count and test inlined prims at extremes
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:26:37.940308+03:00"
---

Problem: test/compiler/native-session.f pins the child's printed HIR-WORD:SESSION-ROWS as a literal (87, now 94 after the seven inlining rows of 77d1e372), so every vocabulary growth moves a literal by hand; test/compiler/native-hir.f already asserts the structural equality rows = HIR-WORD:WORDS, which is the fact. And the new tier-1 expansions of mod and max (test/compiler/tier1-inline-prims.f) are pinned at ordinary values only, not at cell extremes (MIN-INT mod -1, max at MIN/MAX-INT); both bodies are sdiv/msub so agreement is expected, not measured; the decoders' refusal paths (N>EXPAND out of range, EXPAND@ through a wrong meaning, DO-EXPAND E-NELAB-BUNDLE, EXPAND-PAIR E-NELAB-UNDER) are unexercised. Acceptance: native-session derives the count from HIR-WORD:WORDS in the fixture (formatting the number in the child), extreme-value cases for mod and max comparing the engine primitive against the expansion, and one refusal case per decoder path, all green on the head engine. Files: test/compiler/native-session.f, test/compiler/tier1-inline-prims.f. Verify: the two suites with the test/compiler/aot-mode.f prefix. Depends: none. Ownership: compiler tests. Claim: unassigned.
