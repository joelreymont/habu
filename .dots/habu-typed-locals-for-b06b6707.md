---
title: Typed locals for family types
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T09:01:34.993202+02:00"
---

Static invariant: a {: x:type :} local annotation should be expressible for every public checker type, including parametric/layout family types; the boundary is the locals type parser (LOC-ADD name:type tokenizer in src/core/checker.f) which today only accepts base tokens. Evidence: ': ZLT-L ( zlt<n,n> n -- n ) {: x:zlt<n,n> y:n :} y ;' fails with 'unknown type :} in signature' — the annotation parser does not consume the <...> argument list, so the family tail eats following tokens. Consequence: any local binding a layout value must stay bare with a typed-local-lint: allow-bare-local exception (first site: test/type-layout-lower-pending.f TLP-LOCAL, staged for TFAM 12 slice 3). Fix: teach the locals annotation parser the same family-token grammar as signature parsing (resolve through the active package, arity-checked, family-id stored), reusing SIG-FAM resolution; then remove the allow-bare-local exceptions at layout sites and add positive/negative fixtures (wrong arity, unknown family, cross-package private) to test/type-decl-suite.f.

UPDATE 2026-07-10: width-1 ENUM locals expose a second required layer after
token parsing. Even `( fam -- fam ) {: x:fam :} x` rejects on the current
fixpoint although the same family works on the data stack and through typed
`ptr fam` storage. The capability must cover locals binding, lookup, lowering,
snapshot/rollback, and diagnostics while preserving the exact family id. Scope
the first executable slice to non-linear W=1 layouts; W>1 and linear layouts
remain fail-closed under their owning capabilities. Acceptance adds executed
enum identity/use fixtures and negative dtype-as-layout/foreign-family binds,
then removes all corresponding bare-local exceptions. This blocks the
semantic-role-safe Model IR migration in habu-cad-adt-swap-7bf0bb1f.

PROGRESS 2026-07-11 (slice 1 landed): W=1 non-linear layout locals + arity-0
cell-family locals. Design: LOCAL-TYPE resolves a bare arity-0 family tail via
SIG-FAM?; an enum-tier layout (W=1 sum/enum/1-field-product) builds the
asserted one-cell hidden term (MK-PARAM + 0 MK-HIDDEN) which LOC-ANN stores
DIRECTLY into LOCTV (hidden never binds a var — the pinned LAYOUT-BLOCK rule);
LOC-BUNDLE-BIND unifies the captured bundle's tag term against any non-var
LOCTV with a first-failure DEXP/DACT/UF>DIAG capture, so wrong-family binds
emit the standard E-MISMATCH packet with family fields, then stores the
CONCRETE group term (reads restore the exact term; LOCW=1 scalar push; MATCH
from a local read works). Arity-0 CELL families assert the nominal scalar
param like a signature. Linearity: fail-closed by construction — W=1
sums/enums are payload-free (never linear) and linear layouts never expand
into locals (item 12 invariant); LIN-LOCAL-BIND-CHECK unchanged. The wave-C
enum-local reject is consciously LIFTED with executed fixtures (identity,
MATCH both arms, derived-eq through reads, branch reads, 1-field product).
REMAINING TAILS (fail-closed as named unknown-annotation rejects, pinned):
(1) parametric spellings {: x:fam<..> :} — needs the sig <args> grammar in
the annotation path + open-arg semantics; (2) W>1 layout annotations — needs
LOCW>1 assert-and-store against the whole expanded group (the bare-local
LOC-BUNDLE-BIND storage already handles the width; only the annotation-side
assert is missing); (3) arity>0 cell tails; (4) the allow-bare-local
exceptions in test/type-layout-lower-pending.f stay until (1)+(2) land (their
sites are W=2 parametric bundles).
