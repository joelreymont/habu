---
title: "Checker capability: derive eq/order/hash for ADTs"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-03T23:43:23.706621+02:00"
blocks:
  - habu-checker-capability-typed-a480c423
  - habu-checker-capability-layout-4e7f1f03
---

Convenience gap from type-family review: docs/type-families.md:1763 lists automatic deriving of equality/order/hash as a v1 non-goal, so every maki ADT hand-writes comparisons. After TFAM 15/16 land, add opt-in derived words (e.g. DERIVE eq order hash inside SUMTYPE/PRODUCT blocks) generated from family/variant metadata like constructors: checked effects, no trust rows, exhaustive over variants, hidden fields never exposed. Reserved-token + replay + public-signature treatment identical to generated constructors (PLAN.md items 5, 8, 13 patterns). Depends: TFAM 8, 15, 16.

---

## DESIGN 2026-07-10 (fable-cap, derived typed equality)

Static invariant this dot restores: two layout values of the same family may be
compared for equality ONLY through a family-specific, checker-generated word
whose effect is metadata truth — never by binding a bundle to the scalar `=`
prim. Today `=`/`0=` on a layout value fail closed (WALL-2); that is sound but
it means the SKEY replay table cannot key on ADT-value identity. This dot adds
opt-in derived equality (and hash/order) and keeps the scalar-`=` reject as the
negative regression.

### Fresh fail-closed evidence (current fixpoint)

`( cpdt cpdt -- bool ) =` -> reject(0); `( cpres<n,n> -- bool ) 0=` -> reject(0);
`( cpdt cpdt -- bool ) =` for a width-1 enum ALSO rejects. Pinned upstream:
test/type-decl-suite.f TD12-ZEQ. Root cause: `=`/`0=` are scalar prims; a hidden
field cannot bind their var operands (LAYOUT-BLOCK / PARAM-HID).

### §27 non-goal — revisit or scope? (FORK, recommend the scoped path)

- Fork A (revisit §27, overload `=`): make `=` width-aware — a flat W-cell
  equality (tag + payload cells + the deterministically-zeroed padding, which
  TDGEN-BODY guarantees) is provably correct for the default stack-cell-tag
  policy. REJECTED as the primary path: `=` is a hot generic prim; overloading
  it also drags in `<>`/`0=` whose bundle semantics are ill-defined; and it gives
  no hash/order (the replay table needs hash). Keep `=` scalar.
- Fork B (scoped, RECOMMENDED — matches the dot's headline): OPT-IN generated
  per-family words via a `DERIVE eq [order hash]` clause inside the block, e.g.
  `COLOR:EQ ( color color -- bool )`, `SKEY:EQ ( skey skey -- bool )`,
  `SKEY:HASH ( skey -- n )`. Generated exactly like constructors / MAKE/UNMAKE:
  checker-owned metadata effect, certified under a pending window, reserved
  constructor-package treatment (item 8), no trust rows, hidden fields never
  exposed. This is the design.

### Semantics of derived eq

Structural, by-value: two values are equal iff their W cells encode the same
logical value. For the default stack-cell-tag policy, because constructors
zero-fill padding deterministically, this reduces to a FLAT W-cell compare
(tag equal AND every payload/padding cell equal). A payload `ptr` compares as
its pointer bits (identity, NOT deep) — the documented v1 contract (matches
`derive(PartialEq)`; deep eq would need the pointee's own eq). Enum eq is a
single tag compare (W=1). Nested layout fields (Dot 4e7f1f03) compare cell-for-
cell within the same flat image, so a product of enums compares correctly with
no special case.

Order/hash: same flat-cell basis — lexicographic cell compare for order,
a fixed cell-mixing hash (e.g. FNV-1a over the W cells) for hash. The replay
table (SKEY) needs eq + hash; order is optional.

### Checker model

`DERIVE eq` inside a SUMTYPE/ENUM/PRODUCT block records a generated word request
keyed by family-id (parallel to SV.CTOR-SYM). At engine load, generate
`PKG:EQ` / `PKG:HASH` / `PKG:CMP` into the family's reserved constructor package
(TF-CTOR-PKG$; item-8 protection: closed-but-callable WID, no user extension).
The declared effect (`( fam fam -- bool )` etc.) is the metadata truth; the body
is empty and CERTIFIES under a CTOR-PEND!-style window whose expected row is
built from the family's hidden expansion (2W input cells for eq, W for hash),
mirroring CTOR-EXPECTED-ROW. No TRUST/TRUSTED:/set-check in the generated text
(item-8 rule; asserted by the generated-eval capture lint).

### Engine lowering

Add EM-ADT-EQ / EM-ADT-HASH legs beside the EM-ADT-MATCH/construct emitters
(habu2.f). EQ lowers to a fixed-shape memory compare loop over the 2W live
stack cells (reuse the EMIT-P2 helper shape / EMIT-P2-COPY loop skeleton) → a
0/-1 bool, popping both bundles. HASH lowers to a W-cell mixing loop → one cell.
Widths are compile-time constants (TFAM-WIDTH@), so every loop shape is constant
— no runtime patching, no new prim. For an ENUM (W=1) EQ is a single compare and
HASH is the tag itself, so the enum slice needs almost no engine code. Emit the
Gforth `bootstrap/cg/forth.fs` mirror in the same slice (byte-identical fixpoint).

The flat-cell basis is sound ONLY while padding is deterministically zeroed —
i.e. the stack-cell-tag policy. GATE `DERIVE` to stack-cell-tag; reject it on
packed/niche/boxed (item 16) with a documented diagnostic until a structural
(MATCH/UNMAKE-based, policy-agnostic) generator lands.

### Migration story (SKEY replay table)

sched-key.f today keys the replay table by STR= over the rendered pipe-delimited
key (SK-ENTRY$/STR=). After this dot: SKEY:EQ over two stored `skey` PRODUCT
values (from Dot 4e7f1f03) fetched via Dot a480c423, and SKEY:HASH for the
table's bucket index, replace the string compare. The durable schedules.rows
stays the SK-KEY$ render (byte-identical) — eq/hash key the IN-MEMORY table,
the string stays the on-disk contract (sched-key-test.f pins it). report verdict
sums and evidence-row products get the same treatment where identity comparison
is needed.

### Hard parts + resolutions

1. Flat-cell soundness depends on zeroed padding — RESOLVED by gating DERIVE to
   stack-cell-tag; packed/niche/boxed reject until structural generation lands.
2. ptr payload = identity eq — DOCUMENTED as the v1 contract, not deep eq.
3. Linear families — DEFERRED: v1 derives eq/hash only for NON-linear families
   (enums, concrete-arg sums/products with no linear con); comparing a linear
   value is semantically odd and the operands would be consumed. Reject DERIVE on
   a possibly-linear family until TFAM-11.
4. Rollback/snapshot — the generated EQ/HASH symbols ride the SAME publish/
   protection/rollback path as constructors (SUMV-CTOR-SYM, REG-EXT-RB, item-8
   WID protection); a rejected DERIVE restores high-water like a rejected sum.
5. Reserved-token / replay / public-signature — identical to generated
   constructors (PLAN items 5, 8, 13); `EQ`/`HASH`/`CMP` are generator-owned
   tails in the family's ctor package, non-reopenable, callable.

### Slice plan + acceptance + cost

- S1 enum eq: `DERIVE eq` on an ENUM generates `PKG:EQ ( e e -- bool )` = tag
  compare. Acceptance: two same-variant enums compare equal, different-variant
  unequal; `=` on the raw enum still rejects; generated word certifies with no
  trust. COST: CHECKER (generation) + tiny engine (width-1 compare), 1 fixpoint.
- S2 sum/product eq (W>1): flat-cell EM-ADT-EQ + Gforth mirror; gate to
  stack-cell-tag + non-linear. Acceptance: `result<n,n>` and an arbitrary family
  round-trip eq (incl. zeroed-padding variants); nested-enum product eq. COST:
  engine+checker, 1-2 fixpoints.
- S3 hash: `DERIVE hash ( fam -- n )` W-cell mix for the replay table.
  Acceptance: equal values hash equal; used as a SKEY bucket index. COST: both.
- S4 order (optional): `DERIVE order`/`PKG:CMP` lexicographic. COST: both.

### PROGRESS 2026-07-11 (S1+S2 landed)

S1 (enum eq) landed on master; S2 (payload sums + products) implemented on the
same checked-MATCH generator — no engine lowering, no trust rows, no Gforth
mirror. Sums: diagonal double-MATCH (outer binds one value's payloads to
locals, inner the other's; same-variant arms compare CT-INT scalars with `=`
after the widening local bind; cross arms false). Products: UNMAKE both
values, bind fields top-down (enum-typed fields route through their family's
PKG:TAG — that family must also DERIVE eq), compare field-wise. Payload-role
gates at DECLARATION (E-TDECL-DERIVE): pointer payloads reject (no checked
pointer-equality surface exists; `( ptr u8 ptr u8 -- bool ) =` rejects —
evidence-probed), non-CT-INT/linear scalars reject (linear compare consumes;
TFAM-11), parametric families reject (arity-0 only), non-derived enum fields
reject. Products derive EQ only (TFAM-DERIVED-KIND-TAIL? gates the tag tail).
S3 hash remains: no checked cell-mixing over hidden cells, so hash is where
the EM-ADT engine leg + Gforth mirror become unavoidable; the checked-MATCH
eq generator stays the differential oracle for any flat-cell fast path.

### PROGRESS 2026-07-11 (S3 hash landed — SKEY unblocked)

S3 hash landed on design (a): the checked SEMANTIC generator. PKG:HASH
( fam -- n ) = FNV-1a cell fold (DRV-FNV-BASIS $cbf29ce484222325 /
DRV-FNV-PRIME $100000001b3, named constants rendered as hex literals into the
generated text) over the variant tag + bound payload scalars per MATCH arm;
products UNMAKE + fold fields (enum fields via their family's PKG:TAG). Folds
exactly the cells eq compares, so equal-hash-equal holds by construction
(pinned per variant/payload/product shape + non-degeneracy smoke incl.
payload-order). Clause is now an order-free greedy feature list (DERIVE eq
hash == DERIVE hash eq; first token must be a feature; repeats idempotent);
TAG rides any derive on sum/enum; products never get TAG; hash-only families
get HASH+TAG, no EQ. Same payload-role gate, reserved-tail protection
(TFAM-DERIVED-KIND-TAIL? per-bit), PS rows, rollback. CONTRACT: hash values
are in-memory only — the future EM-ADT-HASH flat-cell fast path (S-opt, with
this generator as differential oracle) may change them; nothing persists
derived hashes across engine versions (docs 9.3.2; SKEY keys in-memory,
SK-KEY$ stays the durable contract). Remaining: S4 order (deferred,
E-TDECL-DERIVE pins hold).

### Campaign

Prerequisites (front-matter `blocks:`): habu-checker-capability-typed (compare
STORED keys) AND habu-checker-capability-layout (a product with enum fields to
compare). Recommended order: typed S1 → layout S1 → THIS S1-S3. See the typed
dot's Campaign section for the full ordering and the cad-adt-swap hand-off.
