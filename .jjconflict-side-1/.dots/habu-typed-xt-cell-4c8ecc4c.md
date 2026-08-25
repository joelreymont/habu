---
title: Typed xt-cell array capability
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T15:01:18.271463+02:00"
---

Problem: maki/spec.f's equation registry stores generated einsum RUN-word execution tokens in a slot-indexed cell array (EQ-XT-A) and executes one by index (EQ-XT!). TYPED-VARIABLE (dot habu-typed-xt-cells-08e1dc2c) provides a single persistent monomorphic xt<effect> cell with a fit-checked store and typed fetch/execute, but that is one cell, not an indexed array; the checker cannot yet express a typed xt-cell ARRAY, so EQ-EXEC fetches a raw xt from the array and executes it through a TRUSTED: boundary. The source-local EQ-EXEC boundary retains its rationale, this retirement owner, and focused maki/spec-test.f proof until deletion. Acceptance: extend the checker so a typed xt-cell array can declare its element effect, fit-check stores by index, and recover xt<E> on fetch; migrate maki/spec.f's EQ-XT-A/EQ-XT!/EQ-EXEC to it as the first consumer and delete the EQ-EXEC TRUSTED: boundary; add a checked negative fixture proving a wrong-effect store or an untyped fetch/execute is rejected. Files: src/core/checker.f, maki/spec.f, maki/spec-test.f. Verify: bin/hb --load maki/spec-test.f and the focused checker/package gates. Depends: none. Ownership: checker typed-xt-array capability plus maki/spec.f migration. Claim: unassigned.
