---
title: Clear the remaining reserved-name definitions in lib and src
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-02T14:50:44.753414+02:00\""
closed-at: "2026-10-14T19:34:45.570050+03:00"
close-reason: "Implemented in 94c0e22f: remaining non-engine reserved definitions were renamed or removed, the engine-language exception is documented, and lint, focused tests and converged image checks passed."
---

Problem (fold 339 sweep, $HOME/.cache/tmp/kestrel-r4-dupdiag/f3/sweep-after.out): tools/reserved-name-lint.f still flags src/lib/genio.f:493 public GENIO:TYPE (docs/genio.md says it mirrors 'type'; check.f src/lib/genio.f rc 1 today) and src/habu/primitive-registry.f:29 private FIELD (baked; check.f rc 1); the engine-provided src files (checker.f, sumtype.f, enum-decl.f, layout-buffer.f, decl-event.f, cell-effects.f, compiler/native/checker-owner.f, family.f, habu/xref.f) define reserved words check.f never lints. Acceptance: GENIO:TYPE renamed (callers, src/lib/genio-test.f, docs/genio.md), primitive-registry's FIELD renamed (rebuild, g1 == g2, two-gen); for the engine sources, either rename or state in docs/forth.md's reserved-words rule why engine-provided definitions are exempt (they define the language); tools/reserved-name-lint.f over lib tools test examples gives 0 findings apart from stated exemptions (test/compiler/integer-literals.f refusal fixtures, test/native-unit-compile-e2e.f's loader-shadow cases). Files: src/lib/genio.f, src/habu/primitive-registry.f, docs/forth.md.
