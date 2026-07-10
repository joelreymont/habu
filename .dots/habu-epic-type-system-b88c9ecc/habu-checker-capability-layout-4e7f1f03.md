---
title: "Checker capability: layout-kinded product/sum fields"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T03:09:22.923186+02:00"
blocks:
  - habu-checker-capability-typed-a480c423
---

Gap proven during cad-adt-swap audit (2026-07-10, probe P6): a PRODUCT/SUMTYPE field/payload typed as another layout family (e.g. FIELD d dtype where dtype is an ENUM) rejects at declaration (E-TDECL-PAYLOAD). docs/type-families.md §18 keeps v1 params cell-kinded (rejects option<result<n,n>>) and the FIELD/payload grammar rejects family applications, so the literal 'SKEY as a PRODUCT with enum fields' shape (dot habu-cad-adt-swap-7bf0bb1f priority 1) cannot be authored: a product cannot hold dtype/layout/align enum fields. Needs layout-kinded parameters/fields (§18 'Later syntax' PARAM a layout) so a layout family can be a field of a product or a payload of a sum, with nested width accounting + hidden-field expansion. Depends: TFAM 15 (PRODUCT), TFAM 16 (packed/boxed layout). Related: habu-checker-capability-typed-a480c423 (storage), habu-checker-capability-derive-23788e95 (eq).

---

## DESIGN 2026-07-10 (fable-cap, layout-kinded fields)

Static invariant this dot restores: a product field (or sum payload) declared
with a layout family type must carry that family's IDENTITY into the product's
hidden-field layout — so a `dtype` field cannot be read/written as a `layout`
field, and UNMAKE yields the field already typed as its enum. Today the
DECLARATION parser fails closed (E-TDECL-PAYLOAD) on such a field; that is a
MISSING capability, not a bug. This dot adds it; the reject stays as the
negative regression for the still-unsupported cases (linear-nested, recursive).

### Fresh fail-closed evidence (current fixpoint)

`PRODUCT cpnest 0 FIELD d cpdt FIELD l cplay ;PRODUCT` (cpdt/cplay ENUMs):
declaration rejects, family high-water UNCHANGED (rolled back) — the transactional
TDECL-RESTORE removes the failed rows. Diagnostic: "bad product declaration
'cpnest': unknown payload type at 'cpdt'". Root cause: sumtype.f TDECL-PAY-ELEM /
TDECL-LETTER accept only positional letter params, n/f/r cons, and `ptr T`; a
layout-family token is none of those → E-TDECL-PAYLOAD.

### Why this is the RIGHT scope (not full layout-polymorphism)

This is NOT habu-checker-capability-layout-9b8540bd (fully layout-POLYMORPHIC
type PARAMS, `span<result<n,n>>`, §27 non-goal #3). This dot only allows a
CONCRETE layout family as a field/payload TYPE (`FIELD d dtype`), which the
schema-node substrate (SC-APP, SC-LAYOUT) and the width function already
anticipate (docs §8). Layout-polymorphic params stay out of scope.

### Semantics

Nesting is by-value: a product with an enum field is `slot0 .. tag` where the
enum's tag cell sits at the field's offset. Width recurses (docs §18):
WIDTH(product) = Σ WIDTH(field); an enum field contributes 1, a nested product
contributes its own Σ, a nested sum contributes M+1. v1 depth = ONE level of
concrete layout nesting (enum-in-product first); recursive/self-referential
nesting is boxed (TFAM 16), out of scope.

### Checker model

1. Payload/field grammar (sumtype.f TDECL-PAY-ELEM): after the letter/con/`ptr`
   arms, add a LAYOUT arm — resolve the token via TFAM-SIG-RESOLVE (signature
   scope: own package + unique public); if it is a layout family, emit an SC-APP
   schema node carrying the resolved family-id (+ arg roots; v1 enum = arity 0,
   so SC-APP fam 0 0). Reuse SCHEMA-APP (type-schema.f). Reject a
   possibly-linear nested family in v1 (see Hard Parts).
2. Width + slot accounting (sumtype.f CHECKER-DEFPRODUCT-BODY, TDECL-PRODUCT-FIELD):
   TODAY PF.SLOT = field index and TFAM-SLOTS = field COUNT (assumes 1 cell/field).
   CHANGE: PF.SLOT = cumulative CELL OFFSET; product SLOTS = Σ field widths;
   the per-field width comes from the field schema (T-WIDTH of the resolved
   type). TFAM-WIDTH@ is unchanged (it reads SLOTS). This is the one invasive
   registry-semantics change; audit every PF.SLOT reader.
3. Hidden-field expansion recursion (checker.f LAYOUT-PUSH-FIELDS / MK-HIDDEN):
   for a product, walk its PF rows; a scalar field emits one hidden cell keyed
   by the PRODUCT family + offset; a LAYOUT-kinded field emits the NESTED
   family's hidden cells, each carrying the LEAF family's identity + a composed
   slot (so UNMAKE types that field as `dtype`, and CAP-B positional typing
   catches a dtype/layout swap as a STORED/typed property). PARAM-HID-OK? / the
   pairing path compares leaf family + composed slot.
   FORK — nested hidden identity:
   - Option A (flatten to product-family cells): loses semantic-role safety
     (a dtype cell == an align cell). REJECTED — defeats the dot's purpose.
   - Option B (leaf-family-identified hidden cells) — RECOMMENDED. Delivers the
     dot's priority-1 acceptance ("a swapped dtype/layout field is a CHECKER
     diagnostic") as a typed property of the stored/destructured record.
4. Constructor rendering (sumtype.f TDGEN-SCH): render a nested layout-family
   token in the MAKE/UNMAKE sig text (`( … dtype … -- prec<..> )`). Empty bodies
   still certify because PUSH-LOGICAL already expands a layout family token in a
   signature to its hidden cells, so the declared physical width equals the
   generated word's cell count. No engine constructor change beyond the width.

### Engine lowering

MAKE/UNMAKE are physical no-ops over their field cells (docs §9.4); with a
nested enum field the field is still just cells at an offset, so MAKE/UNMAKE
lowering is UNCHANGED — the only engine-visible effect is the wider product
width, which the existing item-12 pass-2 width-aware machinery already handles
(TFAM-WIDTH@ drives WF facts and transport/store lowering). No new EM-ADT leg.
Gforth mirror: none beyond the shared width path.

### Migration story (SKEY, the headline target)

sched-key.f SKEY becomes:
`PRODUCT skey N  FIELD dt dtype  FIELD lay layout  FIELD al align  FIELD tgt n
 FIELD eng n  FIELD ptxas n … ;PRODUCT` with dtype/layout/align as ENUMs
(the DT-*/LAY-*/AL-* int constants become real enums, or parallel enums per the
swap's Option A). SKEY:MAKE/UNMAKE + field accessors (checked compositions over
UNMAKE) give typed field access; a swapped dtype/layout at assembly is a checker
diagnostic (CAP-B, now on the stored record, not just a transient boundary).
The stored record needs habu-checker-capability-typed (store the product into
the replay table) and habu-checker-capability-derive (typed eq/hash for the
table). SK-KEY$ stays the single durable render (byte-identical; sched-key-test.f
pins it). Also report evidence rows (product with a verdict-enum field) and
model-ir nodes (product-of-indices with enum op-kind/dtype/layout fields).

### Hard parts + resolutions

1. Nested hidden-field identity — RESOLVED (Option B above): leaf family-id per
   hidden cell, composed slot; pairing compares leaf + composed slot.
2. PF.SLOT semantics change (index → cell offset) — REAL: audit every reader of
   PF.SLOT and product SLOTS; add a fixture that a scalar-only product's offsets
   still match today's indices (width-1 fields ⇒ offset == index, so no
   regression for existing products).
3. Linear-nested — DEFERRED: v1 rejects a layout field whose nested family is
   possibly-linear (LAYOUT-MAYBE-LINEAR? on the resolved field type); enums are
   non-linear, so SKEY is fine. Re-open after TFAM-11 whole-bundle counting.
4. Rollback/snapshot — a rejected nested-field declaration must restore the
   registry high-water (TDECL-MARK/RESTORE already covers TFAM/SUMV/PF/SCH/ROOT;
   confirm the new SC-APP nodes are inside the marked range — they are, appended
   in the same TDECL-RUN transaction).

### Slice plan + acceptance + cost

- S1 enum-in-product (the SKEY unblocker): layout arm in the field grammar for
  ENUM families; cumulative width/offset; nested hidden expansion for enums;
  MAKE/UNMAKE render. Acceptance: `PRODUCT prec 0 FIELD d dtype FIELD l layout
  ;PRODUCT` declares; PREC:MAKE/UNMAKE certify; UNMAKE yields `dtype layout`
  (typed); a swapped-field assembly rejects; unknown-family and linear-nested
  reject; scalar-only products unchanged. COST: CHECKER-mostly (sumtype.f
  parser + width/hidden), small engine (width only), 1-2 fixpoints.
  DEPENDS: habu-checker-capability-typed only when the product is STORED (SKEY
  is), so land alongside that dot's S1.
- S2 non-enum layout fields (bounded-depth product/sum fields). COST: both.
- S3 layout-kinded SUM payloads (verdict `fail<reason>` where reason is an
  enum). COST: checker.

### Campaign

Prerequisite: habu-checker-capability-typed (storage) for the STORED use.
Consumer: habu-checker-capability-derive (a product with enum fields to compare).
Recommended order: typed S1 → THIS S1 → derive S1-S3. See the typed dot's
Campaign section for the full ordering and the cad-adt-swap hand-off.
