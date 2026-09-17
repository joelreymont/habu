---
title: Pin internal names gone on the product, kept on whitebox
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:17:55.793578+03:00"
---

Problem: test/internal-word-gate.f's 26 ASSERT-INTERNAL cases expect the product to answer 'hb: internal engine word: <token>' rc 70, which presumes the sealed word's record still ships, and 15 test files reference sealed-internal words by qualified name outside the prefix: engine-suite, checker-effect-authority, defer-history-child, type-family-suite, prefix-mark-test, field-proj-boundary-prepare, generated-declaration-transaction-suite, compiler/ir-id, compiler/ir-id-schema, compiler/native-prefix-rollback, compiler/native-tape-owner, bootstrap-wide-memory-src, bootstrap-wide-tick-src, bootstrap-wide-interpret-src, internal-word-gate (strip lane, 2026-09-17, derived by diffing base.names against strip.names for the 1962 rows that lost their name). Once sealed-internal names are stripped (habu-strip-the-names-89d6524a) the product answers undefined instead and the whitebox host, which has no seal, answers defined. Acceptance: internal-word-gate pins both facts per case, undefined on the product image and present-but-internal on the whitebox host (a WHITEBOX-SUITE item), and each of the 15 files either runs as a WHITEBOX-SUITE or reaches its subject through a public probe the way checker.f's CHECK-QUIET-CANDIDATE! does; test/run.f green on the current engine and on a strip-rule engine built from 512bd6da rebased on the head. Files: test/internal-word-gate.f and the 15 above, test/gate-stdlib-cases.f. Verify: test/run.f on both engines. Depends: none. Ownership: gate harness. Claim: unassigned.
