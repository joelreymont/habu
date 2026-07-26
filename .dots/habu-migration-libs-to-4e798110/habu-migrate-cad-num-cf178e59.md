---
title: Migrate CAD-NUM to unified ENUM
status: active
priority: 1
issue-type: task
created-at: "2026-07-15T06:41:06.206007+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Full context: lib/cad-num-types.f:68 and :80 still declare nominal numeric roles with removed TYPEFAMILY/SUMTYPE syntax, while docs/forth.md permits only STRUCTURE and ENUM after the hard cutover. Cause: the CAD-NUM implementation landed before the unified declaration DSL and no migration leaf owns these new files. Fix: after unified checker/compiler lowering, replace every CAD-NUM legacy declaration and constructor/MATCH consumer with compact or payload ENUM as appropriate, preserving CAD-NUM package API, nominal role separation, tags, zero semantics, arithmetic effects, and TRUSTED boundaries; delete all legacy tokens without compatibility aliases. Acceptance: exact legacy-token census is empty in CAD-NUM sources/tests; existing positive behavior and cross-role checker negatives remain exact; bootstrap, snapshot, AOT, hash, and fixpoint identities are deterministic. Files: lib/cad-num-types.f, lib/cad-num-types-test.f, lib/cad-num-arithmetic.f, lib/cad-num-arithmetic-test.f, TRUSTED.md only if boundary conditions change. Verify: focused CAD-NUM suites, migration lint, typed-local diff lint, bootstrap/fixpoint, host/filemap/trust/full gates.

Claim 2026-07-26: agent=mig-e1 workspace=.jj-ws/habu-mig-e1. Execute with the landed program recipe (.blackboard/migration-plan-20260726.md): full-mode payload ENUM for numeric-result 1 (ok a plus six payloadless rejects), spelling preservation via calibrated verdict tables (consumers across lib/vector, lib/memory, lib/byte-buffer, lib/cad-num-arithmetic, maki/model-ir-test are spelling-preserved - run their suites), REFLECT registry pins per R7, FIELD-removal kill, non-zero discipline, name-cliff precheck. Note lib/ scope: the owning gate is test/run.f, run it.
