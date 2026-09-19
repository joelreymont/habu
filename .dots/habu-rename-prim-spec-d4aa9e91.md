---
title: "Rename PRIM-SPEC's FIELD and lint prims.f"
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T00:03:49.505365+03:00"
---

Claim: alder. The reserved-name CLI now takes explicit paths; the existing
reserved-name fixture suite scans both primitive source files. The error-code
lint already walks every .f/.fs file under src/, so both files are covered there.

Validation: the new fixture fails two assertions on the old FIELD definition;
renamed ROW-FIELD and unchanged SLOT pass the source lint. Whole owning rows
tool-boundary-lints, shadow-lint, prim-parity, tail-pure-fixtures,
native-window-owner and primitive-trust pass in a private tree; the last two
use a freshly built whitebox engine. Error-code lint: 1097 files, 0 findings.
Three product generations are byte-identical, SHA256
536a9d9beceb013e7d24c022d459b67621bed14b01c97aae06f9f05d9fcf65e7.
Check-only Gforth bootstrap passes. Astra review clear. build-fixpoint-fixtures
is deferred to Hazel's controlled gate because it invokes install --force,
which is forbidden in this lane. No full gate or shared engine writes here.

Problem: tools/reserved-name-lint.f reports 'E-RESERVED-DEFINITION src/habu/prims.f:127: FIELD is reserved by parser/control dispatch' (parity lane, 2026-09-17): the table's private row-field accessor (habu-specify-the-engine-fcbcee25 worker 1) reuses a reserved name, and src/habu/prims.f is in no gate's lint list, so nothing caught it. Acceptance: the word renamed (ROW-FIELD or the like) with its SLOT sibling checked for the same, and src/habu/prims.f and src/habu/prim-ref.f added to the lint lists that apply to prefix and post-hook sources (reserved-name-lint, error-code-lint if relevant; not shadow-lint's snap list) so a future row-table edit is linted; the lint clean on the tree. Files: src/habu/prims.f, tools/reserved-name-lint.f (its file list), test/gate-stdlib-cases.f if a lint suite lists files. Verify: bin/hb --load tools/reserved-name-lint.f --; byte fixpoint (prims.f is baked); test/run.f. Depends: none. Ownership: primitive table. Claim: alder.
