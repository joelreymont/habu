---
title: Type field-owner checker surface
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T06:29:36.031905+02:00"
blocks:
  - habu-consume-registry-events-efe7fe5e
---

Problem: TYPE-FIELD-OWNER is a pre-hook package, so post-hook DECL-EVENT
currently reaches its lifecycle through TRUSTED shims. The field-owner candidate
adds more shims. Removed PF-BEGIN, PF-ADD, PF-PUBLISH, PF-RELEASE, PF-FINALIZE,
PF-COMMIT, and PF-ROLLBACK are merely unknown, so CHECK returns uncheckable
instead of rejecting.

Required result: add exact checker PPRIM effects for TYPE-FIELD-OWNER OPEN, ADD,
PREPARE, COMMIT, FINALIZE, ROLLBACK, and TX-SCHEMA-FOR. Convert every DECL-EVENT
field-owner call to an ordinary checked qualified call and delete all DEV-FLD-*
TRUSTED shims. Add the seven retired PF names to the checker's canonical
always-reject token set before signature or primitive lookup, using exact
case-folded token matching. Do not add an alias, compatibility word, broad
prefix match, TRUSTED boundary, or runtime lookup.

This leaf and habu-own-product-field-86660116 land atomically because checker
axioms must never precede their runtime words. TYPE-FIELD-OWNER owns the runtime
API. CHECKER owns its static model and retired-token rejection.

Acceptance: checked wrappers around all seven qualified operations certify with
their exact roles. Wrong arity, pointer role, or phase argument rejects. Every
retired PF name returns checker verdict 0 and interpreter E-UNDEFINED. Removing
any PPRIM row or retired-token row fails a focused production checker fixture.
The trust inventory loses the DEV-FLD shims and gains no replacement.

Files: src/core/checker.f, src/core/decl-event.f, focused type-field owner tests,
and TRUSTED.md.
Smallest real check: production CHECK-CANDIDATE! over the qualified API and all
seven retired names.
Depends: habu-consume-registry-events-efe7fe5e.
Ownership: static TYPE-FIELD-OWNER API model and retired PF rejection only.
Claim: unassigned; implement atomically in the field-owner workspace after the
registry-event package lint dependency lands.
