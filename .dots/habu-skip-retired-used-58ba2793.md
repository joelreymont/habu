---
title: Skip retired used publics in the checker
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.352398+03:00"
---

Problem: after `undefine` retires a used package's public word, the engine's used-search skips it and binds the global (or the one live public), but the checker's `CHECKER-USED-SYM` (src/core/checker.f) finds the retired symbol through `SYM-FIND`/`SYM-VISIBLE` and refuses the bare tail as E-USING-SHADOW-GLOBAL (7141) or E-USING-AMBIGUOUS (7144), or certifies against the retired effect where the engine says E-UNDEFINED. Measured by the fix-tt lane (test/top-row-warn-test.f `TW-SHADOW`, `TW-AMB` drive exactly this).
Acceptance: the checker's used-search answers what the engine binds for a retired used public: the same word, or E-UNDEFINED when the engine has none; a checked definition calling the bare name certifies against the bound word's effect. Tests: the two shapes from top-row-warn compiled in a checked definition (certifies, runs), and a retired public with no global (refused as undefined), in the using suite.
Files: src/core/checker.f, test/using-test.f; test/top-row-warn-test.f comments if the tracker cases change meaning.
Verify: using-test, top-row-warn, outer-interpret; gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
