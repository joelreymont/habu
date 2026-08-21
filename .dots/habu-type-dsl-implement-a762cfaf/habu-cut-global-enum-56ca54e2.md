---
title: Cut global ENUM to unified frontend
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-23T01:00:03.342498+02:00\""
closed-at: "2026-07-26T08:59:10.773255+02:00"
close-reason: "Implemented, reviewed, merged: landed as 32182617b6e8 (Bind global ENUM to the unified front end) plus f8d3dccd2cae (plain-ENUM declaration census), both ancestors of master@origin. The census ran every plain ENUM declaration through the unified path with identical-or-ruled-improved results; the single ruled-improved case is 7107 E-SYNTAX over legacy 7110 for a full-mode keyword in compact position. The legacy-definer-lint wording was amended at closure to the authoritative reading (census tool plus bootstrap-mirror keyword list). F10 recorded: implemented against a then-unclaimed dot during the solo-orchestrator shift; retroactive claim and this closure recorded together."
---

Problem: the public global ENUM token still executes the legacy compact parser/generator in src/core/sumtype.f, so new declarations that spell ENUM do not use the unified declaration event, field provenance, atomic constructor publication, or payload grammar. Required result: after named constructor generation lands, bind the sole global ENUM word directly to the unified frontend normal path, delete the legacy compact ENUM parser/generator branch, and keep SUMTYPE/PRODUCT untouched until their own migration. Plain compact ENUM remains source-compatible; payload ENUM becomes available through the same token. Update verify-source/replay/bootstrap manifests and the existing legacy-definer lint so plain ENUM is classified as unified while SUMTYPE/PRODUCT remain legacy. No alias, alternate ENUM-DECL entry for consumers, syntax heuristic, or dual parser. Acceptance: a token-aware census runs every existing plain ENUM declaration through the unified path with identical family identity, tags, constructors, MATCH effects, derives, package visibility, snapshot/AOT/fixpoint bytes, and diagnostics; payload declarations publish all constructors atomically; mutations restoring the legacy branch or bypassing the unified generator fail. Verify: compact and payload ENUM suites, constructor and MATCH suites, declaration rollback/injection, checker replay/all-errors, verify-source parity, bootstrap codegen, AOT, snapshot, exact fixpoint twice, legacy-definer lint, typed/package/host/filemap/dot gates, and full native gate. Ownership: the global ENUM dispatch and removal of its obsolete legacy implementation only.

Amended at closure (2026-07-26): two recorded rulings correct this text. First,
the "existing legacy-definer lint" named above never existed; the authoritative
reading is that the intended artifacts are the plain-ENUM declaration census
tool itself (tools/enum-census.f) plus bootstrap-mirror-lint's keyword list,
which stays as-is because unified ENUM remains an ADT declaration outside the
recovery corpus. Second, the census acceptance clause reads "identical or
ruled-improved": the one deliberate diagnostic change is `ENUM x variant ;ENUM`
now rejecting with 7107 E-SYNTAX instead of legacy 7110, ruled an improvement
because a full-mode keyword in compact position is a form confusion, not a name
reservation; test/type-decl-suite.f carries the attribution.

Claim: agent=claude-solo workspace=.jj-ws/habu-enum-cutover. Recorded
retroactively at closure: the implementation ran during the solo-orchestrator
shift against this then-unclaimed dot (cutover final review finding F10), so
the claim and the closure are recorded together in this metadata wave.
