---
title: Refuse closing an outer using inside a package
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T09:17:24.957933+03:00"
closed-at: "2026-10-01T09:56:18.027363+03:00"
close-reason: "refused by name: the measured case exits 104 ENGINE-ERROR:USING-OUTER at ;using on product 1d64aa52 (base e11cab55: 67 E-USING-SHADOW-GLOBAL at T); outer-interpret 115 cases agree, using-test ok, gen2 = gen1"
---

Problem: `using` state is rolled back with the package scope (docs/forth.md "Importing a package's public words with using"), so a file that opens `using X` before `package P` and closes it with `;using` inside P gets X back in scope at `;package`. In a single file the leak ends at end of load, but `tools/build-fixpoint.f` concatenates sources into one stream, so the using reached every later appended file. Measured on engine b4e05778: `package QA public 1 constant ZZQ ;package using QA package RB ;using ;package 2 constant ZZQ : T ( -- n ) ZZQ ;` refuses E-USING-SHADOW-GLOBAL at T; with `using QA` inside RB it prints 2. src/habu/prof.f had this nesting; batch 6 gave PROF-ABI a public that collides with a lib/errors.f global, and five gate rows (build-fixpoint-fixtures, aot-chain-producer, build-fixpoint-source, pre-trust-defer, cold-runtime) went red until prof.f closed its usings after `;package`.
Acceptance: a `;using` that would close a using opened outside the current package scope is refused by name (engine, the Habu loop in src/habu/packages.f, and the checker's using mirror agree), or `;package` keeps the closure; pick the rule docs/forth.md states and make all three routes follow it. No file in the tree relies on the old behaviour.
Files: src/habu/habu2.f (`;using`, `;package`), src/habu/packages.f, src/core/checker.f using mirror, docs/forth.md, test/outer-interpret.f and the package suite.
Verify: the case above on both routes; rebuild, chain, gate.
Ownership: krait (Intel lane).
