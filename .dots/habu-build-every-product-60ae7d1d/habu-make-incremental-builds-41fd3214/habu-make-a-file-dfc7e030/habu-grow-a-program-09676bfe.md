---
title: "Grow a program's registries in its build space"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T16:19:15.461934+02:00"
closed-at: "2026-11-09T16:19:55.274243+02:00"
close-reason: "landed with 'Grow a program's registries in its build space'. linear's --repl build (257 DEFLINEARs) writes hb, rc 0; with the reopen input it prints 42 and refuses XM with E-LINEAR-OWNER; a warm rebuild of the subject (2.8 s) equals cold after codesign --remove-signature, maps equal; catch-stale, linear, include-refusal, whole-source-read PASS; next-hb from g1: h1, h2, h3 each 10.3 s, h2 == h3, maps equal, h2 runs 1 2 + ."
---

Problem: a --repl program declaring more types than the checker's boot stores hold (test/linear-suite.f SAVE: 257 DEFLINEARs) failed to build, rc 74: 'linker: lim.f: the cell at space +3230744 in CT-NAME-A-P +0 holds 4451680256, outside hb's data and the build's space'. The program's own definers run inside the build and grew its registries (CT, then SYMS, then TFX) into fresh pages through REG-GROW1, which no file object can name; commit plxkxsry only sized the boot stores for hb's built-ins. Acceptance: a store whose boot buffer lies in DATA grows with CHECKER-REG:GROW, which grows one in an open build's space at the space's here (stores of numbers, as REG-PERSIST-MOVE requires) and any other through REG-GROW1; linear's --repl build writes hb, whose reopened LIM mints and erases and refuses LIM-X. Files: src/core/checker.f, src/core/type-family.f, src/core/type-schema.f. Verify: linear passes; warm rebuild of the linear subject equals cold; next-hb fixpoint. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-a2a
