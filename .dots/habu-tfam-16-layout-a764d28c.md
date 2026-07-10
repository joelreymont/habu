---
title: "TFAM 16: layout policies"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.961669+02:00"
---

PLAN.md item 16. Parse/validate POLICY; stack-cell-tag required default; invalid/unsupported/recursive layouts reject with documented diagnostics; packed-tag, niche-null, boxed as separate checked extensions with layout tests before public exposure. Boxed policy is the maki recursive-IR unlock - see maki adoption epic. Gate 17p. Depends: TFAM 9-15.

## AUDIT (engine lane, on fable 3b37156b)

Current state — what already exists vs what is missing:

- Layout-policy TYPE surface is DONE. `src/core/type-family.f:24-29` defines
  `TL-STACK-CELL-TAG 0 / TL-PACKED-TAG 1 / TL-NICHE 2 / TL-BOXED 3 / TL-CUSTOM 4
  / TL-MAX 4`. The TFAM record has `TF.LAYOUT`; accessor `TFAM-LAYOUT-POLICY@`
  (:201), validating mutator `TFAM-LAYOUT!` (:232, rejects <0 or >TL-MAX with
  E-TFAM-KIND). `TFAM-DECL` (:303) already writes `TL-STACK-CELL-TAG` as the
  default (:320) for every family. So acceptance "missing policy defaults to
  stack-cell-tag" is ALREADY satisfied structurally.
- The `POLICY` CLAUSE GRAMMAR is MISSING. Declaration parsers in
  `src/core/sumtype.f` — `CHECKER-DEFSUM-BODY` (:288), `-DEFENUM-BODY` (:328),
  `-DEFPRODUCT-BODY` (:445) — read `name [arity]` then go straight to variants
  /fields. There is no `POLICY <name>` parse. Docs §22.3 spells the surface
  `SUMTYPE option 1 POLICY niche-null` (POLICY right after arity, before first
  VARIANT). `POLICY` is NOT reserved (not in `TDECL-KEYWORD?`:90 or
  `TDECL-RESERVED?`:127). PLAN:440 says item 16 reserves `POLICY`.
- FAIL-CLOSED TODAY (no soundness hole; this is a capability add, NOT a checker
  miss — Static-invariant protocol does not apply). Writing `POLICY x` today
  makes `POLICY` the first token where a `variant`/`field` is expected →
  `TDECL-SUM-VARIANTS`:268 throws E-TDECL-SYNTAX "unexpected token in sum
  declaration". Rejected, but with a misleading generic diagnostic.
- The packed ABI descriptor registry (`LAY-*`, type-family.f:605-660: LAY.POLICY
  /SIZE/ALIGN/TAGW, LAY-ADD, LAY-FIND, LAY-POLICY@) already exists and is what
  packed-tag/niche/boxed will populate (docs §22.2). Foundation slice does NOT
  touch it.
- Reject-diagnostic channel: declarations reject via `TDECL-THROW` (sumtype.f:41)
  with an offending token + why-string + named class (E-TDECL-SYNTAX 7107 …
  E-TDECL-NAME 7110). A new `E-TDECL-POLICY` (7111) slots in identically.

Dep flags:

- TFAM-13 (item lane landing now = compact diagnostics / declaration-error
  PACKET shape): NO HARD DEP for the foundation slice. Policy rejects ride the
  SAME `TDECL-THROW` declaration-context path every existing TDECL reject uses
  ("empty sum", "missing arity"). The richer JSON ADT-field packet is TFAM 13's
  concern; the new E-TDECL-POLICY class joins it when it lands, no rework.
- Recursive-layout reject (`invalid layout policy for recursive sum`, docs §24):
  NOT EXPRESSIBLE IN v1 → defer to the boxed follow-on. A recursive payload needs
  a self-referential family reference, which the payload grammar cannot produce:
  `TDECL-PAY-ELEM` (:226) only admits letter params / n·f·r cons / `ptr T`, and
  product S1 fields (`TDECL-FIELD-FAM?`:393) resolve only arity-0 width-1
  NON-self families (a self ref isn't registered during its own body →
  unresolved → E-TDECL-PAYLOAD). Boxed (docs §22.4) is the ONLY policy that
  admits recursion, so the recursive reject is inseparable from boxed and lands
  with it, not in the foundation.

## RECOMMENDED FOUNDATION SLICE

Scope (POLICY parse + validate; stack-cell-tag the only accepted policy):

1. Reserve `POLICY` (add to `TDECL-KEYWORD?` so it cannot be a family/variant
   /field name), and add `E-TDECL-POLICY` (7111).
2. Parse an optional `POLICY <name>` clause after the arity token in
   `CHECKER-DEFSUM-BODY`, after the name in `CHECKER-DEFENUM-BODY`, and after
   arity in `CHECKER-DEFPRODUCT-BODY`. Factor one shared
   `TDECL-POLICY ( -- policy )` cursor helper (peek next token; if `POLICY`,
   consume it + the policy-name token and map it; else default). Set the mapped
   policy via `TFAM-LAYOUT!` on the fresh family id.
3. Policy-name map: `stack-cell-tag` → TL-STACK-CELL-TAG (accepted, explicit
   form == the default). `packed-tag` / `niche-null` / `boxed` → recognised but
   UNSUPPORTED → reject E-TDECL-POLICY "layout policy not yet supported: <name>"
   (PLAN risk: physical-layout policies must not be exposed before lowering
   support). Any other token → reject E-TDECL-POLICY "unknown layout policy:
   <name>". Missing clause → default stack-cell-tag (unchanged).
4. Docs: fold the accepted grammar + the two documented reject strings into
   docs/type-families.md §22 / §24 (§24 already lists "invalid layout policy for
   recursive sum" for the boxed follow-on).

Red-first fixtures (test/type-family-suite.f, existing FID/T= harness):
- explicit `POLICY stack-cell-tag` accepts, `TFAM-LAYOUT-POLICY@` == TL-STACK-CELL-TAG.
- missing clause still defaults to TL-STACK-CELL-TAG.
- `POLICY packed-tag` / `niche-null` / `boxed` each REJECT (E-TDECL-POLICY, unsupported).
- `POLICY bogus` REJECTS (E-TDECL-POLICY, unknown).
- `POLICY` used as a variant/family name REJECTS (reserved).
- POLICY clause parsed for enum + product headers too (accept stack-cell-tag, reject others).

Follow-on slices (each its own dot, layout tests before public exposure):
- packed-tag: LAY-ADD ABI descriptor + constructor/match/stack-op/invalid-tag
  lowering. THIS is the maki capability store/fetch upstream key (their v1 uses
  stack-cell-tag; packed-tag deferred to item 16) — build next to unblock them.
- niche-null: single-cell null/non-null repr, requires a non-null type/capability
  (docs §22.3) — do not make implicit.
- boxed: `ptr fam-box` repr + recursive payload grammar + the recursive-layout
  reject; maki recursive-IR unlock.

CLASSIFICATION: capability addition, path already fail-closed. Byte-fixpoint x2
required (sumtype.f is in the checker prefix). No new trust rows.

## FOUNDATION SLICE — LANDED

Implemented in `src/core/sumtype.f`: `E-TDECL-POLICY` (7116 — 7111-7115 were
already taken by checker.f E-CTOR-PROTECTED/E-EXPORT-*, so the coordinator's
"7111" would have collided; used the next free code), `policy` reserved in
`TDECL-KEYWORD?`, and a shared `TDECL-POLICY`/`TDECL-POLICY-SET`
/`TDECL-POLICY-DEFERRED?` clause reader (one-token peek via the existing `PK!`
push-back) called after arity in the sum/product bodies and after the name in the
enum body. Map: `stack-cell-tag` -> TFAM-LAYOUT! TL-STACK-CELL-TAG (explicit ==
default); packed-tag/niche-null/boxed -> "layout policy not yet supported";
anything else (incl. `custom`) -> "unknown layout policy"; bare POLICY -> "missing
layout policy name". All ride the existing TDECL-THROW declaration packet (no
TFAM-13 dep). Docs §22.0 (grammar) + §24 (reject strings) updated.

Fixtures in `test/type-decl-suite.f`: stack-cell-tag accept + default on
sum/enum/product, all unsupported/unknown/missing/reserved rejects, and a prose
packet assertion. Positive fixtures are declared PRIVATE (package `tpol`) because
a PUBLIC family publishes constructors and each consumes one protected-WID seal
slot; the suite already sits at the ~16/session seal cap, and a 17th public family
trips the guard (silent exit 84). That is a pre-existing seal-subsystem defect
unrelated to POLICY — filed as dot `habu-seal-protwid-cap-6f1c9d2b` (likely
downstream of `habu-aot-protected-wid-08716547`). Private families exercise the
POLICY parse fully (visibility-independent) without touching the cap.

Gates green: byte-fixpoint x2 stable; test/run.f PASS (37.5s, stray-unexpected 0,
label-dup 0); seven type suites ok; maki ok; error-code/filemap/host-lint 0
findings; trusted-inventory strict + typed-local-diff-lint exit 0; no new trust
rows. Follow-on slices unchanged: packed-tag (maki store/fetch upstream key),
niche-null, boxed (+ the recursive-sum reject).

## PACKED-TAG — AUDIT + SUB-SLICE 1 (descriptor computation)

Audit finding (de-risks the arc): packed-tag keeps the STACK representation as
cells (docs §4/§22.2 — stack width W identical to stack-cell-tag), so there is NO
stack-codegen lowering on habu's side — constructor/MATCH/stack-op behave exactly
as the default. packed adds ONLY a memory ABI descriptor (`LAY.SIZE/ALIGN/TAGW`);
the buffer marshalling that consumes it is maki's separate capability
(`habu-checker-capability-typed-a480c423`). So the accept-flip is low-risk once
the descriptor is correct — no "lowering is a bigger dependency" fork. The LAY-*
registry (type-family.f) already STORES size/align/tagw but nothing COMPUTED
them (existing LAY tests use hand-written placeholders); that computation is the
deliverable.

SUB-SLICE 1 (landed): pure descriptor computation in `src/core/type-family.f` —
`PACKED-NARROW` (smallest u8/u16/u32/cell tag width for a K-variant count),
`PACKED-TAGW` (0 for tag-less products), `PACKED-ALIGN-UP`, `PACKED-ALIGN`,
`PACKED-DESC ( fam -- size align tagw )`. v1 payloads are cell-kinded, so offsets
are implicit (slot i at i*CELL) and size/align/tagw fully specify the ABI; tag
placed after payload; SIZE = aligned array stride. NO grammar change, NO accept
(POLICY packed-tag still rejects) — nothing exposed to maki yet, so the ABI
conventions are still cheap to revise. Unit tests in `test/type-family-suite.f`
(PACKED-NARROW thresholds + PACKED-DESC on private enum/sum/product). Docs §22.2
specifies the ABI (the contract maki reads).

ABI conventions defined this sub-slice (flag for maki review before consumption):
tag-byte-width = ceil to u8/u16/u32/cell by variant count; tag placed AFTER the M
payload cells; payload cells stay 8-byte / align 8; SIZE = align_up(M*CELL +
tagw, align) = array stride; ALIGN = CELL when M>0 else tagw.

REMAINING sub-slices: (2, LAST) accept-flip — drop packed-tag from
TDECL-POLICY-DEFERRED?, map it in TDECL-POLICY-SET to TFAM-LAYOUT! TL-PACKED-TAG +
PACKED-DESC → LAY-ADD at declaration, with layout tests (private families) proving
the packed family carries the right LAY descriptor; gated on sub-slice 1. Optional
later: mixed narrow-width payload tier (explicit payload-offsets table, needs a
LAY-REC offsets field).

## NICHE-NULL + BOXED — AUDIT (which next + bounded first sub-slice)

Shared machinery both hit (unlike packed, which kept the stack width): both
collapse the STACK width to 1 (docs §22.3/§22.4). W is computed by the single
policy-INDEPENDENT `TFAM-WIDTH@` (type-family.f:226: sum/enum→SLOTS+1,
product→SLOTS, else 1). Its value flows into every transport/linear/MATCH
consumer (`PARAM>FAM TFAM-WIDTH@` in PUSH-LOGICAL/LAYOUT-PUSH-FIELDS/XG-READ-GROUP
/LAYOUT-LINEAR-COUNT and the MATCH tag-on-top = slot W-1 read). So a W→1 collapse
is a one-line edit at the source but a soundness-delicate change at MATCH: a W=1
family has no inline tag cell, so the discriminant must move off the (now absent)
tag slot. That "W=1, no inline tag, discriminant elsewhere" MATCH routing is
SHARED by niche and boxed — build it once.

### niche-null
- Qualifying layouts (docs §22.3): NARROW — only the null-pointer niche, i.e. a
  2-variant sum `{ none (empty), some (single non-null pointer) }`; none=null,
  some=non-null, W=1. Docs forbid the implicit arbitrary-pointer niche ("Require
  a non-null type or capability"), and the unused-enum-tag niche is not in v1.
- Checker model: W→1 via a TFAM-WIDTH@ policy branch; MATCH discriminant = null
  test on the single pointer cell (not a tag read); ctor: none→store null,
  some→store the ptr. Touches the SAME width machinery the linear/transport/MATCH
  slices use → churn + soundness risk (a forged null in `some` breaks the null
  discriminant, hence the mandatory non-null type).
- PREREQUISITE: a `nonnull-ptr<a>` refined pointer type the v1 grammar/checker
  lacks (a pointer type carrying a checker-tracked non-null invariant, sourced
  from a capability/constructor). That is itself a meaty checker-type addition.
- Minimal first sub-slice: the nonnull-ptr type + a `NICHE-QUALIFIES?` predicate
  (recognize the option-shape) — recognition only, no W-collapse, no accept.
  Clean but delivers no niche value alone; gated on the refined-type work.

### boxed  (the maki recursive-IR unlock)
- Needs: (a) inline self-family references in the payload grammar (today
  fail-closed — `VARIANT node ptr tree<a>` → E-TDECL-PAYLOAD "unknown payload
  type at 'tree'"; TDECL-PAY-ELEM rejects family-application payloads, so
  recursion is not expressible and the §24 reject is moot); (b) a boxed
  representation (the whole value is one heap/DATA pointer, W=1, so a self-ref is
  finite); (c) HEAP/DATA allocation + constructor lowering (alloc record, store
  tag+payload, return ptr) — relaxes the project "no heap" only for boxed; (d)
  MATCH deref+tag lowering; (e) layout-cycle detection (mutual recursion). Big,
  and the pieces are entangled (representation ↔ alloc ↔ self-ref ↔ deref).
- BOUNDED first sub-slice (recommended below): recognize a DIRECT self-family
  reference in a variant/field payload and REJECT it under any non-boxed policy
  with the deferred §24 "invalid layout policy for recursive sum" diagnostic.
  Pure grammar + reject — no representation, no heap, no width change, no accept.
  It (i) closes the §24 gap the foundation explicitly deferred to boxed, (ii)
  establishes the self-reference recognition point boxed's accept later hooks,
  (iii) is fully testable (recursive sum rejects with the §24 code). Mutual-
  recursion cycle detection and the boxed representation/alloc/deref are later
  sub-slices.

### RECOMMENDATION: boxed next.
1. Higher value — the maki recursive-IR unlock (the campaign's Model-CAD / IR
   driver), vs niche's narrower one-cell option<ptr>.
2. Its bounded first sub-slice is pure-checker (no heap) AND closes the deferred
   §24 recursive-reject gap — double value at low churn/risk.
3. boxed-first builds the shared "W=1 / no inline tag / discriminant elsewhere"
   MATCH routing that niche then reuses (niche adds only the null-test variant),
   so it de-risks niche.
4. niche is the soundness-delicate one (null-test MATCH) AND gated on the
   nonnull-ptr refined-type prerequisite — better once the W=1 MATCH machinery
   exists and maki's pointer-niche need is concrete.

Boxed sub-slicing: (1) direct self-ref recognition + §24 reject [recommended
first, pure checker]; (2) boxed representation — TFAM-WIDTH@→1 for boxed + accept
POLICY boxed for self-referential families + self-ref lays out as a pointer; (3)
heap/DATA alloc + constructor lowering; (4) MATCH deref+tag lowering; (5) mutual-
recursion cycle detection; (6) full construct/match/invalid-tag/layout tests.

### BOXED SUB-SLICE 1 — LANDED (direct self-ref recognition + §24 reject)
`src/core/sumtype.f`: `E-TDECL-RECURSIVE` (7117 — next free after E-TDECL-POLICY
7116), `TDECL-SELF-REF?` (payload token tail == the declaring family TDN-A/TDN-U),
and a reject in `TDECL-PAY-ELEM` (after the `ptr` recurse, before letter/con) that
throws the docs §24 "invalid layout policy for recursive sum" via the standard
TDECL-THROW declaration packet. Catches inline `tree<a>`, `ptr tree<a>`, bare
`tree`, and product self-fields (product self-refs fall through TDECL-FIELD-FAM?,
which excludes product-kind, into TDECL-PAY-ELEM). NO representation/heap/width/
accept — packed/niche/boxed still reject at the POLICY clause, so every family
reaching payload parsing is stack-cell-tag and a self-ref always rejects here.
Mutual recursion (A→B→A) needs a schema cycle walk — deferred to a later boxed
sub-slice. Reclassifies the pre-existing self-ref test (tdpbad3, product self-
field) from E-TDECL-PAYLOAD → E-TDECL-RECURSIVE (the correct diagnostic); non-self
family payloads (tdpbad1/2/4) stay E-TDECL-PAYLOAD. Fixtures in
test/type-decl-suite.f (4 self-ref forms reject 7117 + a non-self stays 7109 + a
private non-recursive positive). Docs §24 + the foundation TDECL-POLICY comment
updated. Remaining boxed sub-slices 2-6 unchanged.

## BOXED SUB-SLICE 2 (representation) — AUDIT

Q1: can the checker-side representation be prepared WITHOUT accepting the policy?
- The WIDTH branch alone (TFAM-WIDTH@→1 for a TL-BOXED/TL-NICHE family; today
  type-family.f:226 is kind-only, policy-independent) is CHECK-SOUND and
  unit-testable in isolation. The checker models a layout value as a W-cell bundle
  with the tag at slot W-1 (checker.f:1184 "tag on top"; :5177 asserts "hidden tag
  not on group top"; MATCH-SCRUT :6138-6169 walks the W cells WITHOUT reading the
  runtime tag). For a boxed W=1 family, slot 0 is a pointer but the checker treats
  it exactly like a W=1 enum's tag cell — the pointer-vs-tag distinction is
  invisible at check time (linear counts the one cell once = correct; MATCH-check
  reads variants from the registry, not the runtime cell). It only materializes at
  LOWER time (deref vs inline tag read). Testable via the direct TFAM-LAYOUT!
  TL-BOXED mutator, exactly like PACKED-DESC / the LAY unit tests.
- BUT that WIDTH branch is the ONLY cleanly-isolatable piece. The full routing
  (transport/MATCH-check/linear on a REAL boxed value) and the ctor/MATCH LOWERING
  cannot be exercised without boxed DECLARATIONS succeeding, and ctor bodies are
  generated AT declaration from the policy (TDECL-CTOR-WORDS → TDGEN-BODY emits
  "pads + tag ;"), so a post-hoc mutator CANNOT retrofit a boxed (alloc-based)
  ctor. So the representation is only PARTIALLY isolatable: the width metadata yes;
  the routing + lowering no.

Q2: re-scoped ordering (accept moves to LAST, per the expose-after-lowering guard).
- COUPLED block (none independently mergeable/exposable): accept POLICY boxed +
  boxed ctor codegen (alloc+store+return ptr, TDGEN-BODY branches on policy) +
  MATCH deref codegen (habu2.f reads the tag inline at [x19,#-8]; boxed must deref
  the pointer first) + self-ref grammar acceptance (invert s1's reject under
  boxed). accept-without-lowering exposes a broken policy; lowering-without-accept
  has no boxed family to generate ctors for → they land together.
- Cleanly-separable pieces BEFORE the coupled block:
  (a) WIDTH branch [thin metadata, mutator-tested, shared with niche];
  (b) boxed runtime record library — a bump arena over MEM-ALLOC-64K-SPAN + a box
      record layout (tag + payload cells) + BOX-ALLOC / BOX-DEREF-TAG, standalone
      checked words tested DIRECTLY (no boxed family needed). De-risks the coupled
      block's allocator integration + record ABI.
- RECOMMENDED smallest sound next step: (b) the boxed runtime record library.
  Higher value than the thin WIDTH branch — it is the isolatable half of lowering,
  directly testable, and builds on the existing allocator; the coupled codegen
  block then just emits calls to it. (a) is the even-smaller alternative if you
  want the absolute minimum / to bank the shared niche W=1 infra first.

Q3: total boxed lowering size — MEDIUM (tractable), NOT a heap+GC mega-feature.
- Allocator is NOT net-new: MEM-ALLOC-BYTES/CELLS/64K-SPAN (lib/memory.f,
  mmap-backed) exist and are checked-callable. There is NO MEM-FREE (grow-only
  pools), so ARENA ownership (bump-allocate boxed records into a pool, free-all /
  leak-until-reset, no per-node free) is the natural fit — consistent with the
  project "no GC" and it AVOIDS coupling boxed to the linear destructor system.
- Size drivers: (b) record library [small, on MEM-ALLOC]; WIDTH branch [tiny];
  coupled block [medium: accept + ctor codegen + MATCH deref + self-ref grammar];
  mutual-recursion cycle detection [moderate, schema walk — foldable or deferrable,
  direct self-ref already covers the maki tree/IR case]; full construct/match/
  invalid-tag/layout tests. Realistic: ~2 small prep slices + 1 medium COUPLED
  accept+lowering slice (the bulk; not incrementally mergeable) + optional
  mutual-recursion follow-on. PHASE-2-appropriate; the allocator + arena ownership
  are the two facts that keep it medium rather than large.

Design forks (flag before implementing the coupled block):
1. Ownership: arena/pool (recommended — small, no free, matches no-MEM-FREE) vs
   per-node linear free (big, couples to the linear/destructor system). v1 = arena.
2. Mutual recursion: fold into the coupled block or defer as a follow-on.
3. The boxed hidden-field's slot-0 "tag" label is check-harmless but the deref
   codegen MUST gate on TL-BOXED so it never emits an inline tag read for a boxed
   scrutinee (or an inline pointer read for a stack-cell-tag one).

### BOXED SUB-SLICE 2 — LANDED (runtime record library)
`lib/layout/box.f` (checked): a grow-only bump arena of mmap chunks (the
cell-typed MEM-ALLOC-CELLS member of the MEM-ALLOC family; default chunk = 64K of
cells) + box record layout [ tag | payload 0..M-1 ] (tag at cell 0, one-load
deref) + BOX-ALLOC / BOX-TAG! / BOX-DEREF-TAG / BOX-PAY! / BOX-PAY@ (the words the
coupled ctor/MATCH codegen will emit calls to) + BOX-ARENA-RESET (free-all: leak
chunks, force a fresh zeroed chunk). No POLICY accept, no checker width change, no
codegen — purely the reusable runtime half. Ownership is arena/free-all (no
per-node free; the platform has no MEM-FREE), keeping boxed decoupled from the
linear/destructor system. Global pointer state uses the json-write ptr-field
idiom (variable + `X 0 ptr-field`). Unit-tested directly in `lib/layout/box-test.f`
(zero-init, tag/payload round-trip, distinct/independent storage, chunk-boundary
growth survival, arena reset) — no boxed SUMTYPE declaration involved. Placed in
the `lib/layout/` SUBDIR so it is internal boxed-policy runtime, correctly exempt
from the published-stdlib manifest coverage walk (which only requires module rows
for flat `lib/*.f`), like `lib/ptx/`. Registered in FILEMAP + the stdlib gate
(tail-pure-fixtures suite). No engine/prim change → lighter gate (no byte-fixpoint
from this change; bin/hb refreshed only because fable's engine moved under the
lane). Remaining boxed sub-slices 3-6 (heap-alloc/ctor codegen, MATCH deref,
self-ref grammar, accept-flip, mutual recursion) unchanged.

### Item-lane collision (public-signature / repair-diagnostics, tfam-13)
Item lane touches sumtype.f (declaration-DIAGNOSTIC packet shape: c1-doc,
c2-oversize) and render.f (repair-packet rendering). niche/boxed touch sumtype.f
too (payload grammar TDECL-PAY-ELEM + the policy path), but a DIFFERENT region
(grammar vs the TDECL-THROW packet shape) — SOFT overlap, rebaseable, as the
foundation's clean 3-way merge with fable's concurrent sumtype.f refactor showed.
No fundamental collision for the recommended boxed first sub-slice (it edits
TDECL-PAY-ELEM + adds a §24 reject, not the packet shape or render.f). render.f is
touched only at the later niche/boxed logical-type RENDERING sub-slice — flag to
sequence that after the item lane's render.f work lands. TFAM-WIDTH@ lives in
type-family.f, which the item lane does not touch.
