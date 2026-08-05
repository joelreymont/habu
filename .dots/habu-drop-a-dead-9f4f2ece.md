---
title: Drop a dead block argument the selector already sees
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T20:23:12.079860+02:00"
---

src/compiler/native/select.f DDROP? drops a block argument only when the value it carries lives in a data-stack slot AND nothing reads it out of a register. The second half alone is the whole of the safety argument - an argument nothing reads can be left out and the module still says what it said - so the residency conjunct is a SCOPING decision made in habu-keep-a-pass-8025401f, not a soundness one, and it is written down as such in the comment above DDROP?. What it holds back: a block argument that is simply DEAD, carrying a value no operation of the routine ever reads. Removing it is dead-code elimination, which is a different fact with a different owner - the elaborator should not be handing the selector a value nothing wants - and doing it under the name of residency would move hand-built fixtures that pin unrelated rules (test/compiler/native-select.f's join-argument cases were the ones that showed it: their join argument is dead and the emitted operation numbering shifts). Decide the owner first: either the elaborator stops emitting the argument, or the selector's rule is widened to the plain register-need answer and the fixtures are re-baselined with the reason written down. Acceptance either way: no row of any corpus regresses, tools/codegen-compare.f 0 findings, and the native select/regalloc/emit/chain suites green.
