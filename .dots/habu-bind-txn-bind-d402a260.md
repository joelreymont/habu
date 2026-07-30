---
title: Dispatch GPT-2 binding
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T18:09:25.424095+02:00"
blocks:
  - habu-delete-resident-and-05c594cb
---

Why: once the model owns its weight store and `WSTORE:resident` is deleted, the
two validated residency arms still need one public operation that produces the
same model state without duplicating validation or weight access.

Owner and interface: package `GPT2TX` owns a new payload ENUM `bind-result`
with exact arms `bound(FIELD m GPT2TX:gpt2-model)` and
`rejected(FIELD c SAFET:census FIELD code n)`. It owns
`BIND ( SAFET:census MDLCFG:mcfg WSTORE:residency -- GPT2TX:bind-result )`.

Behavior: `BIND` is only an exhaustive dispatcher. It calls `PREPARE`, branches
on the residency, and runs `CHECK` plus `COMMIT-MAPPED` or `CHECK-ALLOC` plus
`COMMIT-ALLOCATED`. One `mcfg` supplies every stage, so the implementation must
not fabricate a foreign-identity late arm.

How a refusal becomes a `bind-result`, stated exactly because the obvious
shorthand is wrong: a `PREPARE` refusal does NOT pass through unchanged.
`maki/infer/gpt2-bind.f` declares three separate ENUM families -
`prep-result` at line 215, `check-result` at 232, and `check-alloc-result` at
258 - and `bind-result` will be a fourth. A value of one family is not a value
of another, so `BIND` must unpack `prep-result.rejected(c, code)` and construct
`bind-result.rejected(c, code)` from its two fields. The census and the code
are carried over unchanged; the result value is rebuilt.

Both `refused` arms are handled for exhaustiveness, and neither is claimed to
be reachable through this entry point. `check-result.refused` and
`check-alloc-result.refused` hand back a live `GPT2TX:prep`, not a census, so
those arms call the already-landed `RELINQUISH ( GPT2TX:prep -- SAFET:census )`
and then build `bind-result.rejected` from that census and the returned code.
Because the same `mcfg` feeds `PREPARE` and the check stage, the identity
refusal `E-GX-FOREIGN` and the defensive no-image refusal cannot arise here.
This dot therefore owns no fixture and no mutation for a late refusal; that
defensive behavior stays proven where it is owned, in the `CHECK` and
`CHECK-ALLOC` suites.

The one failure `BIND` can genuinely surface is an allocation throw out of
`COMMIT-ALLOCATED`. It propagates as a throw, not as a `rejected` arm, and only
after that word's existing cleanup rungs have run: each of `ARENA-STEP`,
`BUF-STEP`, `ATBL-STEP`, and `COPY-STEP` is caught and unwound through
`CA-ARENA-BACK`, `CA-BUF-BACK`, `CA-TBL-BACK`, and `CA-PREP-BACK` with
`FOLD-CODE` before the rethrow, and the census is released exactly once on
every path. `BIND` adds no recovery state of its own and must leave every owner
count restored by that existing cleanup.

Forbidden: `WITH-RESIDENT-SLOT`, weight-byte probes, public resident or store
loans, any new `WSTORE` loan, copied slot walks, new validation, compatibility
surfaces, a two-commit contract, a stale claim requirement, optional
real-artifact success, or masked disposal failure. Weight-byte parity is not
this leaf's business and no future operation is named here as its owner:
`habu-cut-gpt2-model-445a19ff` is authoritative, and model-owned compute may use
the existing `WSTORE:WITH-SLOT` surface internally after the store cut.

Dependency: the sole blocker is `habu-delete-resident-and-05c594cb`.

Acceptance: hermetic mapped and allocated fixtures take their exact arms; a
census returned in a `rejected` result binds successfully on a retry; model
disposal and all live-owner, mapping, table, and allocation counts return to
baseline on success and on refusal; and both residency arms bind the real GPT-2
artifact. Mutations that swap the residency arms, lose the census, skip
`RELINQUISH`, or leak the model fail through the production bind and disposal
paths. The focused GPT-2 bind, allocation, weight-store, and Maki suites pass,
plus the package and typed-local diff gates and the error-code lint if any code
is minted.

Claim: released after rejection of `666a7269`.
