---
title: Reuse indexed dictionary records and binding results
status: active
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.274143+03:00"
blocks:
  - habu-give-a-word-297b990d
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: cedar-indexed-dictionary, `.jj-ws/cedar-indexed-dictionary`, base `0c1ca1f3`.

Own habu1.f WLFIND compiler record wrapper/checked declaration, native/dict.f WL-CANDIDATE/SPELL-REC and binding tests. WLFIND:LENTRY already returns record x12/start x11; consume it instead of indexed search-wl then full XREF rescan. Preserve public search-wl, private/TRUSTED-CELL visibility and retired-wordlist latest-record fallback. Verify precedence/ambiguity, collisions, hide/forget/redeclare and failed-evaluate slot reuse. Keep existing single EFFECT-QUERY; remove further duplicate consumers only if counted. Normal lookup avoids full dictionary scan; measure call counts and all-AOT contribution.375us/intrinsic is historical.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review F01 confirms WL-CANDIDATE discards the indexed record, then XREF-FIND-WL scans to rediscover it. Count visited records and indexed probes by hit/miss/visibility. Compile-time binding/owner/visibility mutation invalidates reuse even within one definition; do not treat a definition boundary alone as authority.

Implementation 2026-09-13: primitive revision `9921b929` precedes its compiler
consumer so the existing native host can build the new primitive before source
calls it. `xref-search-wl` is native DNAME-INT with a trusted-only typed row;
NDICT retains its raw record only inside a private trusted boundary and checks
current visibility on every lookup. Qualified namespace/tail lookups use that
same index. No binding cache or extra effect query was added.

Development evidence: A product `187498da5d9368511492b5e4927932436b205ef473cbd79187229f19b2bdde87`
runs the changed dict source under an isolated package plus the full binding
corpus at tiers 0 and 1. Native raw-boundary rejection tests pass. The corpus
covers actual folded collisions, private/public and using precedence/ambiguity,
compile-time trusted authority and retirement, retained compiled bindings,
latest retired rows, hide/forget and failed-evaluate slot reuse.

Same-process count pair at 15,511 records: `dup` drops from 1 full XREF scan /
15,483 wordlist-field reads to 0 / 1; `NDICT:CALL-TARGET` from 2 / 9,563 to 0 / 1;
an absent qualified namespace from 1 / 15,511 to 0 / 0. Emitted-loop breakpoint
counts on the changed source show 1 index probe for `dup`, 2 for the qualified
hit, 2 for the sampled bare miss, 1 for the namespace miss, and 1 for hidden
`int-mark`; all use zero fallback-scan records. This is operation-count evidence,
not a timing or integrated-product acceptance claim.

Pending: separate review and B product build, focused product tests, root-owned
full compiler/runtime gate and all-AOT timing pair. The recovery mirror emits its
test image, but current stage0 cold-prefix execution stops at unrelated
`atomic-cas` (rc 70) before the fixture; no recovery execution pass is claimed.
