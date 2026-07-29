---
title: Add unique bounded MEM byte borrow
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:00.233981+02:00"
blocks:
  - habu-define-rigid-host-71b010a0
  - habu-use-non-reserved-67821d1c
---

Problem: raw allocated pointers permit unchecked aliasing, copy/drop, out-of-bounds byte access, free-while-live, stale use, and ownership that escapes its mapping lifetime. Fix: reopen package MEM in new lib/memory-region.f and implement `WITH-REGION ( CAD-NUM:alloc-byte-len [ owner<q> -- S ] -- S )` on the existing `MEM:WITH-BYTES` cleanup primitive. Within that lexical scope, BORROW consumes owner<q> to produce one unique transient span<q,u8,e,unique,transient,g>, typed INDEX/C@/C! operate only through bounded evidence, ;BORROW consumes the span and returns the same owner, and FREE consumes logical ownership before callback return. `MEM:WITH-BYTES` performs the physical unmap after the checked callback and preserves its established cleanup and primary-error precedence. No owner, span, raw pointer, index, or generation token may escape the callback. Use PRODUCT values with concrete linear tokens; do not edit lib/memory.f in this leaf. Acceptance: raw index, cross-region, extent/generation mismatch, owner/span copy or drop, free while borrowed, callback return without FREE, post-;BORROW span use, escaped authority, and later-generation index reuse reject; first/last byte access works; negative/index=len throw E-MEM-BOUNDS without modifying sentinels; callback throws still unmap; nested scopes release in reverse order; allocation identity exhausts before reuse. Files: lib/memory-region.f, lib/memory-region-test.f, FILEMAP.md. Verify: exact test load, checker/linear/type-family suites, lib/memory-test.f, refine/trust/host/filemap/dot lints, typed-local diff lint, full native gate.

Edge note 2026-07-17: blocker habu-tfam-11-linear-99fa9990 closed (core
complete); edge repointed to its successor habu-tfam-11b-open-ee9c72c6
(the open-arg lift) conservatively - if this dot only needed the landed
TFAM-11 core, drop the edge at claim time.

BLOCKER FOUND 2026-07-21 (rigid-domains lane, reproduced on the UNMODIFIED engine): the checker cannot cleanly bind consumer type vars across 3+ co-resident fresh atoms in one family application, and a concrete argument between two fresh atoms breaks binding in some slot arrangements. This dot targets span<r,u8,e,unique,transient,g> = >=3 co-resident rigid identities on one owner, which hits the limit. Either model the owner with <=2 co-resident fresh atoms (early slots) or fix the underlying var-binding limitation first. The rigid-domains fixtures (test/rigid-region-suite.f) deliberately stay at <=2 per family for this reason.

NOTE 2026-07-21 (varbind lane, premise-falsification of habu-fix-checker-var-979de7d7): the span<r,u8,e,unique,transient,g> signature is expressible TODAY - the only trap is that r/n/f are reserved scalar letters (float/int/bool cons, checker.f TOK-TYPE) and cannot name type variables. Spell the region identity with a non-reserved letter (e.g. q): span<q,u8,e,unique,transient,g> certifies. 3+ co-resident rigid identities are proven sound (test/rigid-region-suite.f cases 8-12).

Dependency review 2026-07-21: removed habu-tfam-11b-open-ee9c72c6. The
premise-falsification above and the landed rigid-region suite prove that this
leaf needs only the closed rigid-host capability. This common memory type must
land before tensor, model-pack, or inference consumers introduce ownership
surfaces.

Implementation blocker 2026-07-21: the family signature is expressible, but
the PRODUCT generator names its sixth open parameter `f`, which is the reserved
bool scalar token. The generated MAKE effect therefore changes that parameter
to bool. This leaf waits on habu-use-non-reserved-67821d1c; a MEM-local trusted
constructor or retagging shim is forbidden.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `mem-region` and workspace `.jj-ws/habu-add-unique-bounded-527e05ca` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `lib/memory-region.f` does not exist and `rg 'WITH-REGION'` finds nothing outside the dots. The dot stays active and is free to claim.
