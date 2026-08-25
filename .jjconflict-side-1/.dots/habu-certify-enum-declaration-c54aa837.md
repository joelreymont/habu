---
title: Certify ENUM declaration front end
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T22:02:00.448664+02:00"
closed-at: "2026-07-21T22:44:11.497608+02:00"
close-reason: The production ENUM parser now certifies with typed locals and explicit grammar checks; focused and full native gates passed on b925e8e427ab.
---

After the STRUCTURE parser certification refactor, the exact native stage-0 prefix load advances to src/core/enum-decl.f and rejects its independent LETTER-TYPE definition with the same row-preservation failure. Re-derive and factor the ENUM declaration parser so every production word certifies through the real prefix load without widened effects, TRUSTED, set-check, unchecked execution, or runtime guards. Use named character constants and typed locals where those express the existing roles; preserve token spans, variant and field schema semantics, declaration-event order, rollback, diagnostics, constructor generation, snapshot identity, and native/recovery behavior. Add focused regressions for lowercase and uppercase letter types, parameter arity boundaries, ptr recursion, variant field spans, malformed payloads, and transaction rollback, plus an exact production-file certification test. Keep this a separate prerequisite commit from the STRUCTURE refactor and from the atomic declaration transaction. Files: src/core/enum-decl.f and focused tests only unless a reduced checker case proves a soundness capability is missing. Verify exact prefix load, enum declaration/rollback/constructor suites, native fixpoint, bootstrap parity, typed-local diff, trust/package/host/filemap/dot lints, Maki, PTX standard library, and full native gate.

Claim: agent=decl_txn_impl workspace=.jj-ws/habu-atomic-generated-declaration-4c1e8b7a. Land this as a second prerequisite commit after the STRUCTURE certification commit.
