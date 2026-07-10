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
