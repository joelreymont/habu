---
title: Rename the sixteen survivors out of codegen-compare-
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T02:27:55.591785+02:00"
---

master 0de98fca deleted the codegen-compare harness; 16 files keep the prefix of a harness that no longer exists: the five corpus files, migrated{,2..5}, cabi, cc, clang, core, macho, text (all under tools/). Each is now sole owner of a real capability (the programs, the inventory population, the C toolchain, the reference reader, the tokenizer). Rename to a namespace that says what they are (corpus/reference), updating every requirer and the corpus path strings the judge reads as data (judge/corpus1-5.f answer the paths; judge-test.f and judge/src-test.f load them). Cosmetic but wide: ~15 require sites + path literals. Do after ce3ca8fd (migrated de-string) to avoid double-touching those files.

Fold in while touching these files (from the ce3ca8fd landing, 2026-08-19):
1. tools/judge/corpus2.f:15 cites codegen-compare-migrated2.f by LINE NUMBER
   and is already stale twice over - make every cross-file citation in the
   sixteen name-based (word or section names, never line numbers).
2. Possibly-dead requires: all four migrated{2..5}.f carry
   `require tools/codegen-compare-core.f` and `require lib/errors.f` though
   nothing in the converted text names a word from either - verify and drop
   dead ones during the rename, since every require line moves anyway.

Amendments (2026-08-19, post prune landing a2f5cf00): fold-in item 2 (dead
requires in migrated{2..5}) is DONE - drop it. New item: E-CODEGEN-COMPARE-CLOCK
is now thrown from package JUDGE-COST under a CODEGEN-COMPARE name - rename the
code with the prefix cleanup. codegen-compare-core.f no longer exists; the
sixteen are fifteen.
