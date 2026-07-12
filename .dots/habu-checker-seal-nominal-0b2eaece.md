---
title: "Checker: seal nominal families across storage"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T10:12:41.098491+02:00"
blocks:
  - habu-nominal-storage-migrate-7d9997a4
---

Static invariant: an arity-zero TYPEFAMILY value and ptr TYPEFAMILY pointee must never weaken through generic variable/create memory or ptr a and re-emerge as n or another family. Proven checked repros: variable V; : N>ID ( n -- CAD-KIND:target-id ) V ! V @ ; certifies, and : L ( CAD-KIND:node-id -- ptr a ) FP-RID-AT ; : X ( CAD-KIND:node-id -- n ) L @ ; certifies. This forges target/toolchain/region/node identities and bypasses validated private refinements. Root fix: extend checker storage/pointer unification so concrete nominal pointees are invariant across definitions and cannot generalize to ptr a; implement role-typed variable/create/constant defining words or make generic cells reject nominal stores/fetches; allow LAYOUT-BUFFER-equivalent typed storage for arity-zero families. Coordinate with habu-typed-defining-words-aa224eb5, do not add owner runtime guards. Acceptance: minimal negative fixtures reject n->family, family->n, family A->family B, and ptr family->ptr a laundering across separate definitions; typed same-family store/fetch succeeds; numeric generic cells remain valid; diagnostics name expected/actual qualified families; migrate TARGET, TOOLCHAIN, fusion region, Model IR, and tensor owner storage to the sealed facility. Files: src/core/checker.f, src/core/roles.f, defining-word implementation, test/engine-suite.f, docs/effects.md, docs/type-families.md, owner tests. Verify fail-closed exact checked path, focused engine/checker tests, native fixpoint, maki/test.f, full test/run.f.
