---
title: "Checker: type STRUCTURE words"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:14:41.079256+02:00\\\"\""
closed-at: "2026-07-21T17:10:54.473629+02:00"
close-reason: "Landed 5852e5f7: the checker field-projection armed window. Reserved op field-project ( ptr family<args> byte-offset -- ptr field-type ) certifies ONLY inside a name-keyed single-shot window armed by FIELD-PROJ! from the generative crossing (pre-hook internal, sealed like CTOR-PEND!); the committed FIELD ID is the sole authority - owning family, offset, byte extent, role, and instantiated schema all derive from it via TYPE-FIELD reflection with generic substitution from the input pointer's args, and the baked offset is cross-checked against the committed one. Fail-closed default hook; deferred FIELD-PROJ-XT bound in type-family.f mirroring MATCH-PAY-XT. Nine red-first negatives (unarmed, forged offset, past-width, non-layout ptr, foreign family, role confusion, wrong scalar, uncommitted id, seal=uncheckable) + four positives (cell/byte/pointer-role/generic fields projected from MAKE bundles); the two pre-existing layout-pointer rejects stay red. Contract documented in docs/type-families.md section 2.2 - generate-field's re-dispatch binds to it (design in that dot's released claim). Capacity fixes riding the train: lint intern table $400->$800, plus the owed macOS census/CODELEN re-measures (3732, 114444/3852) after spark's literal-split and definer-publication shrinks. Fixpoint x2 (engine 9a9f3724); correctness fully green, perf under waiver 0922330e."
---

Graph repair 2026-07-21: dropped the habu-checker-certify-type-89504a7e blocker.
That edge made the chain circular (certify-type is blocked by prove-generic,
which is blocked by generate-field, which the genfield lane proved is blocked on
THIS dot - the checker field-projection capability is a prerequisite of the
accessor generator, not a follow-up). The certify-type replay work genuinely
follows the front ends; this dot does not.

Own checked effects and negatives for STRUCTURE MAKE, UNMAKE, and field accessors. Instantiate generic field schemas, enforce pointer/value roles and whole-layout widths, and prove malformed or forged access rejects before runtime. Validate declaration, layout, linear, and checker diagnostics slices.

Claim: agent=fieldproj workspace=.jj-ws/habu-checker-type-structure-d996215b (Mac; owns the checker field-projection capability in src/core/checker.f - the sealed schema-aware armed window minting ptr field-type from ptr family + committed byte offset - plus MAKE/UNMAKE effect certification and role/width negatives per the dot. Territory: the projection/armed-window neighborhood (LAYOUT-INTRO/CTOR-PEND/CAST-PEND) and its fixtures; NOT the unification internals.)
