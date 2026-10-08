---
title: "Stop the checker mirroring the engine's scope state"
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T11:08:08.446017+02:00"
---

Problem: the checker binds names through the engine's own lookup (src/core/checker.f LIVE-BIND -> HORIZON-FIND -> the `scope-find` primitive), but keeps state beside it that the engine already holds:
(a) its own copy of the used package names, CK-USE-NAMES / CK-USE-LENS, with three writers (CHECKER-USING driven by the engine's C-USING, the throw-recovery resync CHECKER-RESYNC, and the savepoint restore CK-USE-RESTORE), parallel to the engine's USE-WIDS cells it also reads raw (CK-USED-INDEX, checker.f:12504);
(b) for a using refusal it re-probes every used package's public wordlist itself (CK-USED-MARK, checker.f:12519) to list the packages holding the name, after the engine's lookup already answered SCOPE-FIND-AMBIGUOUS;
(c) a symbol table of its own keyed by package, visibility and name (SYM-FIND checker.f:7295, SYM-INTERN :7357), mapped from the engine's records (CHECKER-RECORD-SYM), beside the engine's dictionary.
Acceptance: (a) and (b) go: the used names come from the engine's own state (wid -> package record), and the candidates of an ambiguous or shadowed name come from the engine's lookup; (c) is measured: what the second table holds that the engine's record cannot, and either the types hang off the engine's record and the table goes, or the reason it cannot is stated as a fact in the dot.
Files: src/core/checker.f, src/habu/habu2.f (C-USING, scope-find), src/core/render.f (UPKG-EACH).
Verify: full suite on the built hb; the using refusal rows (test/using-test.f) unchanged in output.
Depends: none. Ownership: unassigned. Claim: unassigned.

Ruling (Joel, 2026-10-08, supersedes the above where they differ): the compiler core is a small untyped kernel, implicitly trusted and not type-checked; it lives in the system package and is stripped from the delivered hb. No census, no certification ceremony; the kernel is covered by integration tests (external proofs only if ever needed).
Consequence: (c) is dropped (no project to type or re-key the kernel's tables). (a) and (b) stay only as simplification: remove duplicated state where the engine already holds it.
Measured (c), reopened at Joel's request: the checker's symbol table is a second dictionary: interned names with case folding (SYM-FOLD-C, SYM-STR=CI checker.f:6819-6824), power-of-2 growth (SYM-CAP, SYM-GROW :7341), a hash index (HIDX), scope retirement (SYM-RETIRED? :6943; scope exit retires rows :7018), and a per-symbol effect index (USX, checker.f:8508) with truncate/repair on rollback (USX-TRUNCATE). It holds the effect records (USIGS) per symbol, and symbols for names the engine has no record for (primitive rows, axioms, pending definitions). Next: decide whether effects hang off the engine's dictionary record (one dictionary) and the second table, its index and its rollback repair go.
