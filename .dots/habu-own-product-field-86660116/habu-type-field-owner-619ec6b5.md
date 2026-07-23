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

Required result: add exact checker PPRIM effects for `TYPE-FIELD-OWNER:OPEN
( -- n )`, `ADD ( n n n ptr u8 n n n n n n n n -- n )`, `PREPARE ( n -- n )`,
`COMMIT ( n -- )`, `FINALIZE ( n -- )`, `ROLLBACK ( n -- )`, and
`TX-SCHEMA-FOR ( n n n -- n )`. PREPARE returns the validated provisional field
count, so DECL-EVENT can prove event/field contiguity without a raw count read.
Convert every DECL-EVENT field-owner call to an ordinary checked qualified call
and delete all DEV-FLD-* TRUSTED shims. Keep TX-SCHEMA-FOR runtime-live in the
already protected owner wordlist; generated-declaration protection must not
undefine a word whose PPRIM remains active. Add the seven retired PF names to the checker's canonical
always-reject token set before signature or primitive lookup, using exact
case-folded token matching. Do not add an alias, compatibility word, broad
prefix match, TRUSTED boundary, or runtime lookup.

This leaf and habu-own-product-field-86660116 land atomically because checker
axioms must never precede their runtime words. TYPE-FIELD-OWNER owns the runtime
API. CHECKER owns its static model and retired-token rejection.

Acceptance: checked wrappers around all seven qualified operations certify with
their exact roles. ADD pointer-position swaps and non-n phase arguments reject. Every
retired PF name returns checker verdict 0 and interpreter E-UNDEFINED. Removing
any PPRIM row or retired-token row fails a focused production checker fixture.
The trust inventory loses every DEV-FLD row and gains no replacement. Runtime
lookup and the checker agree for TX-SCHEMA-FOR before and after protection.

Files: src/core/checker.f, src/core/decl-event.f, focused type-field owner tests,
and TRUSTED.md.
Smallest real check: production CHECK-CANDIDATE! over the qualified API and all
seven retired names.
Depends: habu-consume-registry-events-efe7fe5e.
Ownership: static TYPE-FIELD-OWNER API model and retired PF rejection only.
Claim: unassigned; implement atomically in the field-owner workspace after the
registry-event package lint dependency lands.
