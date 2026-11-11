---
title: "Merge the interpreter's files into interpreter.f, package INTERPRETER"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T12:45:47.529072+02:00"
---

Problem: package OUTER is the Habu interpreter (numbers, dictionary search, token readers, package keywords, definition heads, the interpret loop) spread over src/habu/outer.f, packages.f, definers.f and interpret.f; Joel, 2026-11-11: 'the name of the outer package makes no sense', 'maybe all these should go into interpret.f and the package should be INTERPRETER and the file name interpreter.f?'. Ruling (Joel, interview, 2026-11-11, chosen option): 'INTERPRETER, one file (Recommended)': 'Package INTERPRETER in one file src/habu/interpreter.f; the four files' code moves as is.' Word names stay the same. Acceptance: src/habu/interpreter.f holds the four files' code in load order (outer, packages, definers, interpret) unchanged; the four files are gone; every require and every comment or doc that names one of them names interpreter.f; the package is INTERPRETER and every OUTER:WORD use and every comment naming the package says INTERPRETER; no consumer names the package (Zen, Tender, Maki masters checked: none). src/habu/repl.f stays its own file. Lanes: (a) the file merge, package name unchanged, brief .jj-ws.noindex/jerry-wasm/.out/briefs/interp-merge-a.md; (b) the package rename on (a). Files: src/habu/, the 36 files that say OUTER:, docs/forth.md, DECISIONS.md, src/habu/AGENTS.md. Verify: engine built twice to a fixpoint from the installed hb, the tests that require the file, then suite and chain once at the combined tip, consumers cold and warm. Depends: none. Ownership: the four files. Claim: agent=jerry workspace=.jj-ws.noindex/interp-merge.
