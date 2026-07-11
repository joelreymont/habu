---
title: "Checker capability: typed ADT arrays + buffer store/load"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T00:25:56.150571+02:00\""
---

Gap found reviewing TFAM coverage for Model CAD (docs/model-cad.md typed backbone). TFAM plan item 16 ships packed-tag/niche-null/boxed as layout POLICIES with stack-level tests, and docs/type-families.md:1276-1282 specifies the packed memory ABI descriptor (tag width, payload offsets, alignment, size) noting it matters for arrays of ADTs - but no TFAM item or dot implements the CONSUMER capability: checked store/load words for layout-family values in buffers, and a typed array-of-ADT container (alloc/store/load/iterate with checked family+args, no hidden-field exposure, no trust rows). Needed by maki: cad-0a report tables, cad-4 schedule measurement history + cache rows, cad-7 artifact cache (serialization builds on this). Until it lands, tables stay parallel-column records per the cad staging rule. Depends: TFAM 15, TFAM 16 packed-tag. Related: habu-epic-adopt-adts, habu-checker-capability-derive.

---

## DESIGN 2026-07-10 (fable-cap, storable layouts — the foundation dot)

Static invariant this dot restores: a layout value's memory image (its W stack
cells) must round-trip through a `!`/`@` (or typed buffer) at a checked address
with the SAME family identity it had on the stack — so a stored tag cannot be
read back as a bare `n`, and a `dtype` cell cannot be stored where a `layout`
cell is expected. Today the checker fails CLOSED on every memory touch of a
layout value; that is sound but it is a MISSING capability, not a bug. This dot
adds the capability and keeps the fail-closed reject as the negative regression.

### Fresh fail-closed evidence (current fixpoint, `bin/hb`)

Probe verbatim (CHECK-QUIET-CANDIDATE!, -1=certify / 0=reject):
- `( cpres<n,n> ptr a -- ) !`   -> 0   (sum W=2 store rejects)
- `( ptr a -- cpres<n,n> ) @`   -> 0   (sum fetch rejects)
- `( cpdt -- ) constant`        -> 0   (enum constant rejects)
- `( cpdt ptr a -- ) !`         -> 0   (ENUM W=1 store ALSO rejects)
- `( cpdt ptr cpdt -- ) !`      -> 0   (typed-ptr store rejects)
- `( ptr cpdt -- cpdt ) @`      -> 0   (typed-ptr fetch rejects)
- `( ptr cpdt -- ptr cpdt )`    -> -1  (a `ptr enumfam` type ALREADY parses/flows)
Pinned upstream: test/type-decl-suite.f TD12-STORE, TD12-CONST, TD12-DEPTH.
Key finding: the addressing type `ptr family` already exists in the signature
grammar and flows as one cell; the ONLY missing piece for width-1 enums is the
checker's acceptance of the store/fetch — the runtime is already a plain cell op.

### Semantics of a stored bundle (default `stack-cell-tag` policy)

Physical stack shape (docs §5): `slot0 .. slot(M-1) tag`, tag on TOP, slot0
deepest; W = TFAM-WIDTH@ = (sum/enum: M+1) / (product: Σ field widths) — enum
W=1 (tag only). Constructors zero-fill padding (TDGEN-BODY emits `0 .. 0 tag`),
so a bundle's W cells are fully determined by its logical value.

MEMORY IMAGE: ascending addresses, slot0 at offset 0 … tag at offset (W-1)*CELL.
So memory order is slot order with the tag LAST (highest address); a store
writes the W live stack cells such that a fetch reproduces `slot0 .. tag` (tag
back on top). This is the exact inverse of the transport COPY/REV loops already
emitted (habu2.f EMIT-P2-COPY). v1 policy: every cell full 64-bit, size = W*8,
align = 8. Narrow tags / payload packing are item-16 packed-tag, OUT of v1
scope (a later slice keyed to that item, not a prerequisite).

Lifetime/GC: none — static language, DATA-allocated, never freed; storing a
non-linear bundle copies bits with no ownership transfer. A payload `ptr`
stores/loads as its pointer bits (identity, no deep copy) — the v1 contract.

### Checker model

Addressing type: `ptr family<..>` (already parses; SC-PTR over a layout
T-PARAM). This is how a stored layout is named without exposing hidden fields.

Store/fetch surface — FORK (recommend A):
- Option A (RECOMMENDED): OVERLOAD `!`/`@`, exactly as item 12 overloaded
  dup/swap for transport. In CELL-STORE-TOK/CELL-FETCH-TOK (checker.f ~4814):
  when the value operand below the addr resolves to an EXPANDED non-linear
  layout group (HIDDEN-PARAM? run, whole W cells present) AND the addr is
  `ptr <same family>`, consume the W-cell group + addr and produce nothing
  (store) / consume the `ptr family` and push the W-cell group (fetch). Reuse
  the XG-READ-GROUP group reader and the WF width-fact surface so emitters get
  the width. Scalar `!`/`@` stay byte-for-byte (the layout arm only fires on a
  layout top-of-group). Rationale: no new surface, mirrors transport, gives
  `variable`+`!`/`@` a memory story for ADTs.
- Option B: new words `L!`/`L@` (or `TFAM-STORE`/`TFAM-FETCH`). Rejected:
  duplicates `!`/`@`, and `variable`/arrays still need `!`/`@` on the addr.

Reject rules (negative regressions, all fail-closed today, must STAY closed for
the bad cases): linear bundle store/fetch rejects (a linear payload laundered
through raw memory — same rule as XG-READ-GROUP's `LAYOUT-LINEAR?` reject);
open-arg layout rejects (width unknown); family mismatch (`ptr dtype` addr with
a `layout` value) rejects via ordinary PARAM-PAIR-ARGS family unification on the
`ptr` inner; a bare `ptr a` addr with a layout value rejects (must name the
family) unless the value is width-1 and the store is explicitly the untyped
one-cell path (see interpret gate).

Interpret wide gate interaction: a word whose effect mentions a stored/fetched
layout is already wider-than-cell → E-ADD-EFFECT sets RECW, DNAME-WIDE marks it,
EM-INTERPRET-FIND fails closed. So store/fetch of a bundle can only appear
inside a CHECKED colon body, never at the untyped interpret level — which is
why the DEFINING words (below) never pop a bundle at interpret level.

Linear counting: unchanged — a stored bundle is consumed exactly once at the
store and produced once at the fetch (LAYOUT-LINEAR-COUNT samples at the tag
cell). v1 restricts store/fetch to NON-linear families; linear-in-memory is a
later slice gated on TFAM-11 whole-bundle counting.

### Defining words / containers

The interpret stack is untyped and DNAME-WIDE forbids bundles there, so a
storable-layout `constant` at interpret level is NOT viable and is NOT proposed
(C-CONSTANT stays one-cell, the locked TFAM-12 verdict). Storage is always
through an ADDRESS reserved by a defining word that never pops a bundle:
- `variable NAME` + `!`/`@`: a `variable` reserves 1 cell and types `-- ptr a`.
  For an ENUM (W=1) this is already enough memory; the checker just needs the
  store/fetch overload to accept `ptr cpdt`/`cpdt`. So a `variable` re-typed via
  a `ptr family` cast/accessor stores one enum cell. (Report/IR/fusion columns
  are `create … cells allot` = 1 cell/entry, so enum W=1 fits with NO layout
  change — see migration.)
- `LAYOUT-BUFFER NAME family count` (new defining word, slice 3): reserves
  W*count DATA cells and generates a typed index accessor
  `NAME ( i -- ptr family<..> )` (checked effect generated from metadata, like
  a constructor — no trust). Element stride = W*8. Store/fetch/iterate compose
  over the accessor + `!`/`@`. This is the typed array-of-ADT container the dot
  headline asks for.

New registry? For the enum/typed-ptr tier NO new mutable checker registry is
needed (the `ptr family` type already carries the family-id). For LAYOUT-BUFFER,
the array metadata is a DECLARATION-time fact; if a small registry is added it
MUST follow the pointer-free, REG-EXT-RB-SAVE/RESTORE + REG-EXT-PERSIST pattern
(type-family.f) so rollback and snapshot stay correct — see Hard Parts.

### Engine lowering

- Enum / W=1: the runtime store IS a one-cell `!`; fetch IS a one-cell `@`.
  No new engine code — the store/fetch is physically what `!`/`@` already emit;
  only the checker gains acceptance. (Slice 1 is CHECKER-ONLY.)
- W>1 bundle store/fetch (slice 2): add pass-2 legs EMIT-P2-STORE / EMIT-P2-FETCH
  mirroring EMIT-P2-COPY (habu2.f ~3585): a fixed-shape memory-cell loop, width a
  compile-time constant from the WF fact on the `!`/`@` token. STORE pops the
  addr then writes W cells (slot0..tag) to [addr..addr+(W-1)*8] ascending; FETCH
  reads W cells from [addr..] and pushes slot0..tag. `!`/`@` join the pass-2
  width dispatch (EM-COMPILE-P2WIDE) and the WF fact recorder (a `!`/`@` variant
  of WF-XPORT-RECORD, operand pos = the group below the addr). Emit the Gforth
  `bootstrap/cg/forth.fs` mirror in the same slice (byte-identical fixpoint).
- LAYOUT-BUFFER (slice 3): the index accessor lowers to `i W*8 * NAME-base +`
  (a scaled address add); a generated checked word, no new prim.

### Migration story (CAD swap targets — stable accessor signatures)

- report G-TAG/G-RO/G-RL (report.f:120-122,:486 `tag gid cells G-TAG + !`):
  make G-TAG hold a `verdict`/`gate` ENUM (W=1). Array stays `cells allot`;
  the store becomes `tag gid cells G-TAG + !` where `tag:verdict` and the
  address is `ptr verdict`; GATE-TAG@ returns `verdict` instead of `n`.
  Slice 1 (enum store/fetch) unblocks this with NO array-shape change.
- model-ir MI-OP/MI-DT/MI-LAY/MI-IS-AL (model-ir.f:66-86,:200): op-kind,
  dtype, layout, align become ENUMs (W=1) in their existing 1-cell columns;
  MIR-OP@/MIR-DT@ return the enum; every consumer that took `n` takes the enum
  (semantic-role safety: MI-DT can no longer be read as a layout). Slice 1.
- fusion FP-SP-REASON (fusion-plan.f:60,:247): split-reason ENUM (W=1) in its
  1-cell column; FP-SPLIT+ stores the enum. Slice 1.
- schedule SKEY (sched-key.f): a PRODUCT record whose fields are dtype/layout/
  align ENUMs — needs Dot 3 (layout-kinded fields) for the product AND this dot
  to STORE the product into the replay table, AND Dot 2 for typed equality/hash.
  The durable schedules.rows stays one render word (SK-KEY$, byte-identical;
  sched-key-test.f:53 pins it) — the stored PRODUCT is the in-memory key, the
  string stays the on-disk contract.
- report evidence rows / schedule measurement history (store.f, arrays of
  product rows): LAYOUT-BUFFER typed arrays (slice 3).

### Hard parts + resolutions

1. Interpret-level bundle storage — RESOLVED: storage is address-mediated;
   defining words never pop a bundle; bundles only cross `!`/`@` inside checked
   bodies (DNAME-WIDE enforces it).
2. Family identity on a raw `ptr a` — RESOLVED: require `ptr family` for the
   typed store/fetch; a bare `ptr a` layout store rejects (must name the family)
   so a stored tag can never be read back as a mismatched family.
3. Rollback / snapshot for LAYOUT-BUFFER metadata — REAL, FLAGGED: any new
   registry rides REG-EXT-RB-SAVE/RESTORE + REG-EXT-PERSIST with pointer-free
   integer/interned-offset records (type-family.f pattern); a rejected buffer
   declaration must restore its high-water like TDECL-RESTORE. No GC/lifetime
   concern (static), but the rollback/snapshot interaction is the one that bites.
4. Linear-in-memory — DEFERRED: v1 rejects linear-family store/fetch; re-open
   after TFAM-11 whole-bundle linear counting.
5. Aliasing / non-atomic W-cell store — single-thread safe; FLAG for threads
   (out of scope; dot habu-threads if it lands).

### Slice plan + acceptance + cost

- S1 enum / width-1 typed store-fetch (`ptr family` + `!`/`@` overload).
  Acceptance: `( verdict ptr verdict -- ) !` and `( ptr verdict -- verdict ) @`
  certify; linear / open-arg / family-mismatch / bare-`ptr a` reject; a fetched
  enum MATCHes; report+IR+fusion enum columns type-check with unchanged array
  shape and unchanged on-disk output. COST: CHECKER-ONLY (+negatives), 1 fixpoint.
  LANDED 2026-07-10 (fable-cap; checker-only as predicted, 1 fixpoint):
  - src/core/checker.f: LAYOUT-MEM-OK? (tier gate: non-linear, W=1);
    LAYOUT-MEM-INNER + LAYOUT-STORE-STEP/LAYOUT-FETCH-STEP wired into
    CELL-STORE-TOK/CELL-FETCH-TOK (the resolved `ptr family` ADDRESS picks the
    arm; effect rows built from the pointee term via PUSH-LOGICAL +
    CHECKER-STEP, so the value side pairs hidden-to-hidden); and
    LAYOUT-PTR-BIND-OK? in LAYOUT-BLOCK? (a var may bind a W=1 non-linear
    layout pointee under CUR-STRICT — PAIR-STRICT's single call site is
    U-TYPE's T-PTR arm and PAIR inherits it, so the flag IS the ptr-pointee
    context).
  - IMPLEMENTATION FINDING (design addendum): the wall was TWO gaps, not one.
    Besides the `!`/`@` arm, the typed ADDRESS was unproducible — binding a
    variable's `-- ptr a` row to `ptr family` rejected in U-TYPE (pre-S1
    probe: `( -- ptr cpdt ) VFOO` -> 0). The pointee-bind relax is the second
    half; without it the mem arm is unreachable from checked code.
  - Pins: test/type-decl-suite.f TDS1-* — positives (enum store/fetch, tuck
    round-trip, zero-arity W=1 sum, closed parametric tdmemu<n>, accessor
    bind, executed store->fetch->MATCH round-trip on tdcolor) and negatives
    (bare `ptr a` both directions, family mismatch, n->enum / enum->n
    laundering, W>1 tdres store+fetch [flips at S2], linear tdmemu<tdown>,
    open tdmemu<a>). TD12-STORE/TD12-CONST/TD12-ZEQ/TD12-DEPTH unchanged.
  - docs/type-families.md §17: S1 status paragraph.
  - Gates: fixpoint refresh OK ("compiler fixpoint"); test/run.f PASS
    (35075ms <= 70000ms); maki/test.f PASS; all 8 type suites ok rc=0;
    dot-dep-lint 0 findings; error-code-lint 0 findings;
    typed-local-diff-lint rc=0.
- S2 wide bundle store-fetch (W>1 sums/products): EMIT-P2-STORE/FETCH + WF facts
  on `!`/`@` + Gforth mirror. Acceptance: store-then-fetch round-trips a
  `result<n,n>` and an arbitrary non-result family; padding preserved; bad-width
  and linear reject. COST: engine+checker, 1-2 fixpoints.
- S3 LAYOUT-BUFFER typed array-of-ADT (alloc / typed index / store / load /
  iterate). Acceptance: array of a product family stores/loads with no hidden-
  field exposure, stride W*8, iterate visits each element as a typed bundle;
  rejected declaration restores registry high-water. COST: engine+checker, 1
  fixpoint.
- S4 packed-tag array interop (item-16 ABI descriptor for narrow tags) —
  DEFERRED, dot-linked to PLAN item 16; out of v1.

### Campaign (all three capability dots)

Storable (THIS dot) is the FOUNDATION. Recommended order:
1) THIS S1 (enum store/fetch) — unblocks report/IR/fusion immediately, cheapest.
2) habu-checker-capability-layout (enum-in-product fields) — unblocks SKEY shape.
3) habu-checker-capability-derive S1-S3 (enum eq → product eq → hash) — unblocks
   the SKEY replay table (typed equality/hash over stored keys).
4) THIS S2/S3 (wide bundles + typed arrays) — report evidence rows, schedule
   measurement history.
Then the CAD ADT swap (habu-cad-adt-swap-7bf0bb1f) executes per its file plan.
Dependency edges (front-matter `blocks:`): derive blocks on {typed, layout};
layout blocks on {typed}. This dot has no capability-dot prerequisite (its
upstream TFAM 15/16 are landed/frozen for this lane).
