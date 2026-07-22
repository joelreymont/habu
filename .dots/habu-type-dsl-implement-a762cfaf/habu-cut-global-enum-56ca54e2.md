---
title: Cut global ENUM to unified frontend
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T01:00:03.342498+02:00"
blocks:
  - habu-enum-generate-named-1f3261a3
---

Problem: the public global ENUM token still executes the legacy compact parser/generator in src/core/sumtype.f, so new declarations that spell ENUM do not use the unified declaration event, field provenance, atomic constructor publication, or payload grammar. Required result: after named constructor generation lands, bind the sole global ENUM word directly to the unified frontend normal path, delete the legacy compact ENUM parser/generator branch, and keep SUMTYPE/PRODUCT untouched until their own migration. Plain compact ENUM remains source-compatible; payload ENUM becomes available through the same token. Update verify-source/replay/bootstrap manifests and the existing legacy-definer lint so plain ENUM is classified as unified while SUMTYPE/PRODUCT remain legacy. No alias, alternate ENUM-DECL entry for consumers, syntax heuristic, or dual parser. Acceptance: a token-aware census runs every existing plain ENUM declaration through the unified path with identical family identity, tags, constructors, MATCH effects, derives, package visibility, snapshot/AOT/fixpoint bytes, and diagnostics; payload declarations publish all constructors atomically; mutations restoring the legacy branch or bypassing the unified generator fail. Verify: compact and payload ENUM suites, constructor and MATCH suites, declaration rollback/injection, checker replay/all-errors, verify-source parity, bootstrap codegen, AOT, snapshot, exact fixpoint twice, legacy-definer lint, typed/package/host/filemap/dot gates, and full native gate. Ownership: the global ENUM dispatch and removal of its obsolete legacy implementation only.
