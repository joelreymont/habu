---
title: Use non-reserved PRODUCT parameter names
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T23:34:12.595349+02:00"
---

Problem: generated PRODUCT constructor/destructor effects use the contiguous letters a..z for positional variables. At arity six the generated MAKE row reaches f, which the signature checker reserves for bool. The same ambiguity exists at n and r. A valid family such as span<q,u8,e,unique,transient,g> is expressible directly, but the generator and declaration parsers do not share a reserved-safe parameter alphabet, so generated effects change meaning as arity grows and block the common MEM types.

Fix: define one canonical positional parameter mapping in the existing type-family owner, with both index-to-token and token-to-index operations. The legal single-letter alphabet excludes f, n, and r. Use that mapping in the SUMTYPE/PRODUCT parser and generator, the STRUCTURE and full ENUM parsers, out-of-arity diagnostics, and every generated declaration/effect path. Set the declaration-language maximum to the size of that alphabet (23) instead of claiming that all 26 lowercase letters are usable; keep the schema index itself positional and unchanged. Update the language documentation and required bootstrap/generated-source owners whose exact prefix shape changes. Do not special-case memory, reinterpret scalar f/n/r by arity, add TRUSTED constructors, or invent a second mapping in a post-hook parser.

Acceptance: generated products at arities 1 through 23 certify; every parameter remains independently polymorphic; constructor/destructor and field round trips preserve exact parameter position; f, n, and r always retain their scalar meanings; reserved scalar tokens never appear as generated variables; repeated generation is deterministic; arity 24 rejects at the shared declaration boundary; generic, linear, snapshot, ahead-of-time compilation, bootstrap, and fixpoint tests pass. Add a red-first arity-six fixture matching the MEM span shape, a maximum-arity round trip, reserved-scalar field controls at arities beyond their old positions, and wrong-position/out-of-range negatives. Migrate the existing arity-eight declaration fixture to the canonical alphabet and prove its final position remains index seven.

Files: src/core/type-family.f owns the shared mapping; src/core/sumtype.f, src/core/structure-decl.f, and src/core/enum-decl.f consume it; docs/type-families.md and focused type-family/type-declaration/structure/enum/constructor suites specify it. Include only required bootstrap/source-list mirrors. Verify generated-effect inspection, the type-family, type-declaration, structure, enum, constructor, and linear suites; typed-local, package, host, and dot lints; ahead-of-time compilation, fixpoint, and the full native gate.

Claim: agent=fork-product-params workspace=.jj-ws/habu-use-non-reserved-67821d1c
