---
title: "Rename PRIM-SPEC's FIELD and lint prims.f"
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T00:03:49.505365+03:00"
---

Problem: tools/reserved-name-lint.f reports 'E-RESERVED-DEFINITION src/habu/prims.f:127: FIELD is reserved by parser/control dispatch' (parity lane, 2026-09-17): the table's private row-field accessor (habu-specify-the-engine-fcbcee25 worker 1) reuses a reserved name, and src/habu/prims.f is in no gate's lint list, so nothing caught it. Acceptance: the word renamed (ROW-FIELD or the like) with its SLOT sibling checked for the same, and src/habu/prims.f and src/habu/prim-ref.f added to the lint lists that apply to prefix and post-hook sources (reserved-name-lint, error-code-lint if relevant; not shadow-lint's snap list) so a future row-table edit is linted; the lint clean on the tree. Files: src/habu/prims.f, tools/reserved-name-lint.f (its file list), test/gate-stdlib-cases.f if a lint suite lists files. Verify: bin/hb --load tools/reserved-name-lint.f --; byte fixpoint (prims.f is baked); test/run.f. Depends: none. Ownership: primitive table. Claim: unassigned.
