---
title: Migrate CAD-NUM to unified ENUM
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T06:41:06.206007+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Full context: lib/cad-num-types.f:68 and :80 still declare nominal numeric roles with removed TYPEFAMILY/SUMTYPE syntax, while docs/forth.md permits only STRUCTURE and ENUM after the hard cutover. Cause: the CAD-NUM implementation landed before the unified declaration DSL and no migration leaf owns these new files. Fix: after unified checker/compiler lowering, replace every CAD-NUM legacy declaration and constructor/MATCH consumer with compact or payload ENUM as appropriate, preserving CAD-NUM package API, nominal role separation, tags, zero semantics, arithmetic effects, and any surviving source-local TRUST boundary with its rationale, this dot as retirement owner, and focused production test; delete all legacy tokens without compatibility aliases. Acceptance: exact legacy-token census is empty in CAD-NUM sources/tests; existing positive behavior and cross-role checker negatives remain exact; bootstrap, snapshot, AOT, hash, and fixpoint identities are deterministic. Files: lib/cad-num-types.f, lib/cad-num-types-test.f, lib/cad-num-arithmetic.f, and lib/cad-num-arithmetic-test.f. Verify: focused CAD-NUM suites, migration lint, typed-local diff lint, bootstrap/fixpoint, host gate, and the full native gate.

Claim 2026-07-26: agent=mig-e1 workspace=.jj-ws/habu-mig-e1 (RELEASED 2026-08-21: workspace gone, no live lane - gc). Full-mode payload ENUM for numeric-result 1 keeps ok a plus six payloadless rejects; calibrated verdict tables preserve consumer spellings across lib/vector, lib/memory, lib/byte-buffer, lib/cad-num-arithmetic, and maki/model-ir-test, whose focused suites run. Keep REFLECT registry pins, FIELD-removal kill, non-zero discipline, and the name-cliff precheck. The owning lib/ gate is test/run.f.
