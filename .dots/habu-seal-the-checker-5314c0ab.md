---
title: Seal the checker into packages
status: open
priority: 2
issue-type: task
created-at: "2026-07-25T16:51:11.577318+02:00"
---

Give src/core/checker.f real package owners so the package ownership gate can stop exempting it, and delete its entry from the complete-file global exception list.

Why this is needed. The exact-diff package gate (tools/package-diff-lint-core.f, word GLOBAL-IMPLEMENTATION?) admits a fixed list of files whose global definitions implement the core language before packages exist. src/core/checker.f was not on that list, so until now every change to the checker failed the commit gate. Evidence, measured on master commit c82a37ca: the four-commit unified declaration stack produced 16 E-PACKAGE-OWNERSHIP findings, all in src/core/checker.f - 10 for new global words (RETIRED, RETIRED-TOK?, RTL-I, LOC-NAME?, RETIRED-GLOBAL?, REJECT-RETIRED, RBF.COORD-OFF, RBF-NO-COORDINATOR, RBF.COORD, RBF-POP-WITH) and 6 for changed bodies of words that were already global on master (DO-TOK1, CHECK-RESET, CHECK-VERDICT, RBF-REC, RBF-PUSH, RBF-POP). A control probe proved the condition is not specific to that stack: adding one trailing comment to the existing CHECK-RESET body on pristine master, changing no behavior and defining no new word, reds the gate with the same error. So the gate rejected every possible checker change.

The interim decision was to admit src/core/checker.f the same way sumtype.f, roles.f, structures.f and enums.f are admitted, because the checker genuinely is a global pre-hook language surface by current construction: the PRIM:/PPRIM: primitive-axiom machinery and the RBF rollback-frame surface are global by design and load before any package exists. That entry is interim and this dot retires it.

What to build. Give the checker real package owners at its natural seams rather than one giant package: the primitive-axiom registry machinery, the rollback-frame (RBF) surface, and the verdict/reset path are three separate concerns and should not share one namespace. Keep the words that must stay global (the pre-hook language surface the engine boot depends on) explicitly identified and justified, and move everything else behind package boundaries with short package-local tails.

Size this honestly as a program, not a leaf. src/core/checker.f is the largest source file in the tree and its global words are called from the engine boot path, the checker hook, the type-family registry, and the generated-declaration machinery, so the caller cascade crosses src/core, src/habu and the test tree. Expect to split it into staged dots: one per seam, each with its own caller migration and its own gate run. Do not attempt it as a single change.

Owner: tools/package-diff-lint-core.f owns the exception list; src/core/checker.f owns the definitions.

Acceptance. The src/core/checker.f entry is deleted from GLOBAL-IMPLEMENTATION? in tools/package-diff-lint-core.f and from the complete-file global exception list in docs/forth.md section Packages, its hostile fixtures in tools/package-diff-lint-test.f are updated to the new expectation, and a diff that adds a new global word to src/core/checker.f is REJECTED by tools/package-diff-lint.f. The full master gate list stays green on that tree.
