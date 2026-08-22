---
title: "one declaration system: retire SUMTYPE and PRODUCT"
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.851213+02:00"
---

Problem: legacy src/core/sumtype.f (2131 lines: NEWTYPE/SUMTYPE/PRODUCT, 22/11/3 non-test consumer files) coexists with the unified STRUCTURE/ENUM stack (~3700 lines, 41/95 consumers); docs/forth.md:620-627 admits the removal half never happened (E-REMOVED-TYPE-SYNTAX exists nowhere). Duplicated by line: payload/field resolver x3 (sumtype.f:357, enum-decl.f:301, structure-decl.f:267), arity parser x3, decimal scanner x4, POLICY+DERIVE clause parsers x3 (enum/structure byte-identical), body collectors x2. TYPE-FIXES-PLAN.md wave-1 ruled the deletion. Acceptance: the 14 SUMTYPE/PRODUCT sites migrate to ENUM/STRUCTURE, TDECL-DEFSUM/TDECL-DEFPRODUCT and their grammar are deleted, the enum/structure front ends share one clause-parser package, E-REMOVED-TYPE-SYNTAX refuses the old spellings with a test. Files: src/core/sumtype.f, enum-decl.f, structure-decl.f, the 14 call sites. Verify: full test/run.f and maki/test.f green; recovery gate green. Depends: 9269e3a3 (prefix crossing). Ownership: declaration front ends. Claim: unassigned.
