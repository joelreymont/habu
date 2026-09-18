---
title: Name the file and line in every load refusal
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T13:13:59.164020+03:00"
---

Problem (aspen for Joel, 2026-09-18, from the LESSONS.md audit): 'hb: bad string literal' printed no file and no line (an S\" literal with a \u escape inside a fixture), so the worker bisected by hand; a refusal that names no location costs the reader the whole search. Acceptance: every engine-side load refusal (the LCOMPILEDIE family, the string-literal and escape refusals, the definer refusals, the parse refusals in src/habu/habu2.f and the source scanner src/habu/verify-source.f) carries the source file and line the way the checker's diagnostics do (the include frame's path and the token's line, via the same origin machinery MEO-APPLY / diag-origin use), measured by a census: list every fd-2 refusal string in habu1.f/habu2.f/jit.f/repl.f and classify it located / unlocated before and after; a fixture per newly located family (the bad-string-literal case red-first: expected 'hb: bad string literal at <path>:<line>'); docs/debugging.md states the format. Files: src/habu/habu2.f, src/habu/habu1.f, src/habu/verify-source.f, test/runtime-regression-test.f, docs/debugging.md. Verify: the fixtures; three generations with cmp; bootstrap check; test/run.f. Depends: none. Ownership: engine diagnostics. Claim: unassigned.
