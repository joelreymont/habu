---
title: Refuse an arity change at definition time
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T13:13:59.170239+03:00"
---

Problem (aspen for Joel, 2026-09-18): a second definition of a name with a different arity passes the source run and is refused only by ncomp minutes later at the native build (-8303 E-NELAB-ARITY); the interpreter should refuse it at definition time. Measure first: the package wordlist already refuses a duplicate tail (E-DUPLICATE-DEFINITION rc 78, docs/forth.md), so find the shape that gets through — a global redefinition? a redefinition after undefine with a different arity while callers were certified against the old one? a public/private pair? — reproduce it through the real load path and the check tool, then refuse it at the layer that first sees the second definition (the definer's duplicate check or the checker's signature record: a certified caller's declared effect against the new signature), with a named code (E-DUPLICATE-DEFINITION or a new E-ARITY-REDEFINITION in the same family), the file and line, red-first fixtures, docs/forth.md's redefinition bullet updated; ncomp's E-NELAB-ARITY stays as the backstop. Files: src/habu/habu2.f or src/core/checker.f (by measurement), test/, docs/forth.md. Verify: the fixtures; three generations with cmp; test/run.f. Depends: none. Ownership: engine / checker, by measurement. Claim: agent=hazel workspace=.jj-ws/hazel-arity-redef.
