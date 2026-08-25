---
title: thirty-four error codes declared in two to five files
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.862049+02:00"
---

Problem: 7107 is declared in sumtype.f:50, structure-decl.f:59, enum-decl.f:81, type-family.f:1845, generated-declaration.f:146; similarly 7101/7102/7108/7110/7116/7119/7122-7128/7161-7177/7190-7191, each file explaining that global pre-hook constants do not survive the fixpoint. engine-error.f already shows the fix: a pre-hook package of constants with PPRIM: rows. Acceptance: one pre-hook codes package, duplicates deleted, error-code-lint 0 findings. Files: src/core/*.f listed, src/core/engine-error.f. Verify: tools/error-code-lint.f exit 0. Depends: none. Ownership: error codes. Claim: unassigned.
