---
title: "Checker: type STRUCTURE words"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:41.079256+02:00"
closed-at: "2026-07-21T18:09:23.095248+02:00"
close-reason: "landed swusykrl: STRUCTURE construct/access already correct via shared product-ctor path; certify suite pins it; TRUSTED rows repointed to seal-set-check"
---

Graph repair 2026-07-21: dropped the habu-checker-certify-type-89504a7e blocker.
That edge made the chain circular (certify-type is blocked by prove-generic,
which is blocked by generate-field, which the genfield lane proved is blocked on
THIS dot - the checker field-projection capability is a prerequisite of the
accessor generator, not a follow-up). The certify-type replay work genuinely
follows the front ends; this dot does not.

Own checked effects and negatives for STRUCTURE MAKE, UNMAKE, and field accessors. Instantiate generic field schemas, enforce pointer/value roles and whole-layout widths, and prove malformed or forged access rejects before runtime. Validate declaration, layout, linear, and checker diagnostics slices.

Claim: agent=fieldproj workspace=.jj-ws/habu-checker-type-structure-d996215b (Mac; owns the checker field-projection capability in src/core/checker.f - the sealed schema-aware armed window minting ptr field-type from ptr family + committed byte offset - plus MAKE/UNMAKE effect certification and role/width negatives per the dot. Territory: the projection/armed-window neighborhood (LAYOUT-INTRO/CTOR-PEND/CAST-PEND) and its fixtures; NOT the unification internals.)

Claim: agent=structcert workspace=.jj-ws/fable-structcert machine=spark (owns checker typing of STRUCTURE construct/access; sibling of the in-flight ENUM certify lane - same-file checker.f edits expected, orchestrator hand-merges)
