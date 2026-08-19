---
title: Rename the sixteen survivors out of codegen-compare-
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T02:27:55.591785+02:00"
---

master 0de98fca deleted the codegen-compare harness; 16 files keep the prefix of a harness that no longer exists: the five corpus files, migrated{,2..5}, cabi, cc, clang, core, macho, text (all under tools/). Each is now sole owner of a real capability (the programs, the inventory population, the C toolchain, the reference reader, the tokenizer). Rename to a namespace that says what they are (corpus/reference), updating every requirer and the corpus path strings the judge reads as data (judge/corpus1-5.f answer the paths; judge-test.f and judge/src-test.f load them). Cosmetic but wide: ~15 require sites + path literals. Do after ce3ca8fd (migrated de-string) to avoid double-touching those files.
