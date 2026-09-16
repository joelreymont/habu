---
title: Retire the audited evaluate in source-generating definers
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:59:57.429065+03:00"
---

Problem: the source-generating definers (deftype, the codegen definers, and the FFI declarer aspen is building) each need one evaluate of generated source, wrapped today in a TRUSTED: body with an audited effect; with TRUSTED: retired they have no honest boundary (aspen declarer lane, 2026-09-16, and the test sweep found 81 evaluate sites of the same shape). docs/forth.md forbids an axiom for arbitrary evaluate. Acceptance: one engine-source mechanism for a definer that generates source: either a checked generate-then-evaluate word whose declared effect is verified at the boundary (the checker certifies the generated text under the definer's declared effect before it runs, and the runtime checks the depth contract), or the definer emits definitions through the compiler API without text; every definer converts to it, the FFI declarer uses it from day one, the TRUSTED: bodies go; fixtures for a definer whose generated source has the wrong effect (refused by name) and one that succeeds. Files: src/core/deftype.f or its owner, src/habu/*codegen* definers, src/core/checker.f, the FFI declarer, docs/forth.md. Verify: the definer suites; test/run.f. Depends: none. Ownership: definers. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.
