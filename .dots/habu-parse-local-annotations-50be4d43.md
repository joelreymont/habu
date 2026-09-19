---
title: Parse local annotations with the signature type grammar
status: active
priority: 1
issue-type: task
created-at: "2026-09-19T18:20:04.421254+03:00"
---

Problem: src/core/checker.f LOCAL-TYPE (~3944-3953) has its own smaller grammar for '{: name:type :}' annotations: a family with arity > 0 is refused ('fam TFAM-ARITY* 0 <> IF a u BAD-LOC-ANN'), so 'observation<localfile>', 'box<n>' or 'tlp-res<n,n>' cannot be a typed local while they are legal in the signature. Acceptance: local annotations are parsed by the signature parser SIG-TYPE (family resolution, <...> parameter lists, nested families, the definition's declared type variables, arity checking) with the parser cursor saved and restored around the annotation, and the local shorthand ':ptr' (inferred pointee) is retained explicitly - the handoff of 2026-09-19 section 4.5 measured that routing through SIG-TYPE breaks it (reference: experiment/wide-typed-locals@origin 9a514f09, 574cf8d8, 7a26fb5b; re-derive here, do not merge); the narrow annotation parser is retired, not kept beside the shared one; fixtures: a parametric wide local bound and returned, a nested family, a declared type variable in an annotation, ':ptr' unchanged, a wrong-arity annotation refused with the existing diagnostic. Files: src/core/checker.f, test/, docs/forth.md. Verify: fixtures; three generations; test/run.f. Depends: habu-bind-a-wide (the arity-0 wide local dot). Ownership: checker. Claim: unassigned. Measured on that branch's CI (run 35446389326, bba129e0): the bootstrap dies before its smoke with 'habu: in install: ptr needs an element type' at 'prior:ptr' - the bare ':ptr' special case (LOCAL-TYPE: a u s" ptr" CORE-STR= IF FRESH MK-VAR MK-PTR EXIT THEN) must stay explicit ahead of the SIG-TYPE delegation.
