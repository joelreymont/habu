---
title: Retire E-SHADOWED-ARITY once own records are read by symbol
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:23:00.063915+03:00"
---

After 7b3f85de (jerry-ownrec: the compiler reads a compiled definition's own effect record by the checker symbol it was written under, NATIVE-DOES-FINISH-SYM, RECORDS-END/RECORDS-RETRACT), src/core/checker.f SHADOW-ARITY-CK and its E-SHADOWED-ARITY refusal no longer guard anything: the by-name lookup they protected (KEEP-ARITY asking by name; a shadowed or private twin answering for the definition) is gone. Acceptance: show by probe on the ownrec engine that no program reaches a wrong arity without SHADOW-ARITY-CK (shadowed public/private twin, qualified definer, redefinition in one package); then delete SHADOW-ARITY-CK, the error code row and its docs/forth.md entry, and turn test/shadowed-arity-test.f into the cases that now pass (or delete it if native-own-record.f already covers them). If a probe still needs the check, the dot instead states what it guards in the code comment. After 7b3f85de lands. Files: src/core/checker.f (baked), the error table, docs/forth.md, test/shadowed-arity-test.f.
