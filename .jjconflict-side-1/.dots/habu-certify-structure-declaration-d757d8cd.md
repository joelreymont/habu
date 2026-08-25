---
title: Certify STRUCTURE declaration front end
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T21:59:03.402868+02:00"
closed-at: "2026-07-21T22:44:11.493298+02:00"
close-reason: The production STRUCTURE parser now certifies with typed locals and explicit grammar checks; focused and full native gates passed on b925e8e427ab.
---

Current master cannot certify its own src/core/structure-decl.f through the native prefix load. A minimal empty-prefix build reaches LETTER-TYPE and rejects the first IF because the declared row must be preserved with a bool, but the branch path exposes ptr u8 n n n. After reducing that site, FIELD-CLAUSE also reaches an uncertified 2>r path. This is independent of the generated-declaration transaction and remains after the reviewed enum payload repair xrnqowqm. Re-derive the typed effects and factoring for the STRUCTURE declaration parser so every production word certifies without widening effects, TRUSTED, set-check, unchecked execution, or local guards. Preserve exact syntax, token spans, schema nodes, declaration-event ordering, rollback, diagnostics, native/recovery/fixpoint behavior, and generated MAKE/UNMAKE semantics. Add a focused prefix-certification regression that loads the exact production file and fails if any word becomes uncertified, plus branch-specific tests for arity letters, built-in types, ptr recursion, field spans, malformed types, and throw rollback. The old habu-checker-type-structure-d996215b dot covered generated construct/access effects, not certification of this parser, so it remains closed. Files: src/core/structure-decl.f and its focused tests; touch checker/compiler code only if a reduced soundness gap proves the source cannot express the invariant. Verify exact prefix load, structure declaration/rollback/make suites, native fixpoint, bootstrap parity, typed-local diff, trust/package/host/filemap/dot lints, Maki, PTX standard library, and the full native gate.

Claim: agent=decl_txn_impl workspace=.jj-ws/habu-atomic-generated-declaration-4c1e8b7a. Land this as a separate prerequisite commit before resuming the declaration transaction.
