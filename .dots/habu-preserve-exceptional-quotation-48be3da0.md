---
title: Preserve exceptional quotation schema roots
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T13:23:34.871162+02:00\""
---

Problem: SCH-QUOT stores only normal A-D rows and HASR, while checker quotation terms carry XHAS/XDEAD and exceptional data/return outputs G/H; family schema persistence, constructors, layout validation and rendering can erase or reject exceptional quotation semantics. Fix: extend schema quotation metadata and roots canonically; validate flags/roots; instantiate/render/copy all normal and exceptional rows; preserve snapshot rollback/persistence; keep rendered public signature behavior explicitly normal-only where required. Acceptance: create/persist/instantiate/render round trips XHAS/XDEAD/G/H; malformed flags/roots fail closed; inferred throwing quotation cannot unify with an explicit normal-only quotation; cycle/linearity/wide walkers traverse exceptional roots. Files: src/core/type-schema.f, src/core/type-family.f, src/core/layout-valid.f, src/core/sumtype.f, checker integration and focused suites. Depends: habu-persist-aot-checker-a1c95dea. Ownership: schema representation/consumers and focused tests; no AOT materializer or envelope edits.
