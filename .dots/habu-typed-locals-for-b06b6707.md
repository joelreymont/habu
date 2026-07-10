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
