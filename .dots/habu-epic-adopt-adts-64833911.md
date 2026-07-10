---
title: "Epic: adopt ADTs across maki"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.966746+02:00"
---

Goal dot: types must be used extensively in maki. After TFAM 9/10 land: option/result in maki host APIs (error paths, lookup returns); enum families for opcodes/modes; after TFAM 15: PTX IR nodes as products; after TFAM 16 boxed policy: recursive IR/autograd-tape/ONNX-graph ADTs by value (until then typed ptr + arena). Prioritize boxed policy inside TFAM 16 by maki need. Convenience gaps to watch: no deriving (eq/hash) in v1, no layout-polymorphic params (see capability dot). Success: maki suite green with ADT-typed public APIs, no new trust rows.

## PHASE 2 SWITCHOVER READINESS — AUDIT (fable b4390e9d)

Foundation status: items 8 (constructors) / 9 (MATCH) / 12 (layout-aware stack
ops) / 14 (enums) / 15 (products) are ALL LANDED. The core ADT migration pattern
works END-TO-END today — verified by executing probes, not just checking sigs.

Probe results (declare a public sum + payload-carrying ctors + return it from a
checked word + MATCH in a caller + EXECUTE):
- option<scalar>/option<idx>: `SUMTYPE optn 1 none | some a` + OPTN:SOME/NONE +
  `( n -- optn<n> )` returner + MATCH caller → runs (some 5→5, none→0). WORKS.
- option<ptr a>: some carrying a pointer, matched + deref'd → runs (some→42,
  none→99). WORKS as a 2-cell [ptr,tag] repr — **niche-null is NOT required**;
  it is only the 1-cell optimization.
- result<a,b> (two params, wave B): `SUMTYPE rslt 2 ok a | err b` + DIVR + MATCH
  → runs (10/2→5, 10/0→-1). WORKS. MATCH refines BOTH arms' payloads onto the
  stack (ok's a AND err's b).
- Payload-carrying constructors work → **boxed is NOT needed for option/result**.

So the TYPE foundation already supports waves A (option), B (result multi-cell),
C (enums, item 14), and D (product, item 15). The gating issues are NOT type
capabilities — they are the WID cap and the public-API surface.

First wave = A (option<scalar>/option<idx> over sentinels). Supported today.
~80 sites (lib/string,date,float,map,process-env + tools/{imgdump,imagedisasm,
date,json,trusted-inventory}); FIND-SUB/INDEX-OF have the widest caller radius
(migrate LAST). census-switchover §5.

BLOCKERS:
1. Protected-WID cap = EXACTLY 16 public families/session (probed: fam1..fam16
   OK, the 17th → SILENT exit 84). This is the batch-mode manifestation of
   habu-aot-protected-wid-08716547 (batch WIDN starts low → after ~16 public
   families it walks into the baked protected WIDs 300/70000 → guard fires),
   tracked also by habu-seal-protwid-cap-6f1c9d2b. It does NOT block wave A's
   FIRST slice (a shared `option` = ONE family), but it caps cumulative PHASE 2
   and is FRAGILE + SILENT → MUST land before PHASE 2 scales past a handful of
   shared families. Blocked on the 2b lane's habu2.f. Hard prerequisite for
   SCALING (not for starting).
2. maki/test.f does NOT exercise ADTs today (grep: zero SUMTYPE/MATCH/option/
   result). The epic success bar (maki suite green with ADT-typed public APIs)
   needs later waves to reach maki APIs; wave A is lib/+tools, not maki.
3. SOFT: public-signature rendering of option<T>/result<T,errno> RETURN types on
   migrated PUBLIC APIs. The item lane is landing constructor-sig synthesis
   (fable: "public-sig: synthesize ENUM constructor signatures"); confirm it also
   renders option/result RETURN sigs before exposing a migrated public API. Not a
   blocker for internal-word first slices; becomes one at the public-API boundary.

NOT blockers (disproven by probe): niche-null (option<ptr> works as 2 cells);
boxed (payload ctors work); item-12 multi-cell layout ops (result<n,n> runs).

RECOMMENDATION:
- First wave: A.
- Bounded first slice: establish the SHARED `option<T>` family in its own file
  (lib/adt/option.f or lib/option.f, loaded before consumers — the load-order
  convention is part of this slice) + migrate ONE LOW-RADIUS option<idx> finder
  (NOT FIND-SUB/INDEX-OF; pick a 1-3-caller finder, e.g. A-FIND-INDEX or a
  map/date/float parser) to return option<idx> + rewrite its ≤3 callers to MATCH
  + a T{ }T test per changed word. Lands the shared family + the migration
  pattern + the load-order convention at minimal radius; ONE public family, so no
  WID-cap blocker.
- Dependency order to START PHASE 2: 8/9/12/14/15 DONE → wave A first slice can
  start NOW. Before wave A COMPLETES / PHASE 2 SCALES: land
  habu-aot-protected-wid-08716547 (the 16-cap fix). Before migrating PUBLIC-API
  RETURN types: confirm the item lane's public-sig renders option/result returns.
