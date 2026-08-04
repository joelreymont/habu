---
title: Resolve checked full namespace paths
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T06:35:00.353571+02:00"
---

Source dependencies: exact reviewed E1 namespace rows, the HB package hard cut,
and the native last-separator contract. Owner: `NAMESPACE` for syntax and
`XREF` for live dictionary resolution.

Reopen package `NAMESPACE` in `src/core/checker.f` and define public `NONE=-1`,
`BAD=-2`, and `SPLIT ( ptr u8 n -- n )`. `SPLIT` returns `NONE` with no colon,
`BAD` for a leading, trailing, or doubled colon, and otherwise the last-colon
index. Add its three exact primitive-effect rows before `PTABLE-END`. Checker
qualification, both type-family constructor/package predicates, type-family
signature qualification, and XREF use this one state machine; delete copied
scanners. The checker's local `name:type` grammar remains separate.

Package `XREF` owns namespace-row helpers and `PARSE ( ptr u8 n -- n )`.
`PARSE` calls `NAMESPACE:SPLIT`, refuses `BAD` with the malformed-name
diagnostic, and returns only `NONE` or a split index. Existing `XREF-FIND` and
dictionary lifecycle entries keep their interfaces, accept package and type
namespace rows, return absence only for a well-formed missing name, and never
collapse malformed input into missing. Reuse the found record rather than a
duplicated raw-index scan. Renderer text specifies nonempty colon-separated
components.

Write set: `src/core/checker.f`, `src/core/type-family.f`,
`src/core/render.f`, `src/habu/xref.f`, `test/engine-suite.f`,
`tools/xref-test.f`, `test/prop-test-core.f`, and
`tools/gate-json-assert-core.f`. Any surviving source `TRUST` keeps only its
source-local rationale, retirement owner, and focused production test. Do not
add a custom result family, namespace-kind table, parent/path side table,
schema, version, compatibility spelling, ancestor lookup, using change, native
emitter edit, or lint.

Acceptance: deep sibling identities stay distinct; valid deep names resolve
through production checker, type-family, and XREF paths; malformed names reject
distinctly from undefined names across lookup, optional/mandatory undefine,
hide, and forget; exact full-prefix diagnostics are retained. Run the focused
engine, XREF, property, and JSON diagnostic tests plus package, typed-local,
fixpoint, and full native gates.

Evidence: Unlanded candidate commit `d0ae5455ec21` is preserved for future adaptation.
