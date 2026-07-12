---
title: "V2 R3: type design revision pass ids"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:31.314585+02:00"
blocks:
  - habu-v2-r3-declare-3fcdeebb
---

Problem: design/revision/object/analysis/plan/pass/schema identities in CAD transaction and pass APIs are represented by strings or raw n and can be interchanged. Fix: migrate the first owner APIs and records to CAD-KIND:design-id, rev-id, obj-id, analysis-id, plan-id, pass-id, and schema-id; raw decoding/refinement stays private and validated. Acceptance: every pairwise adjacent-role negative fixture rejects; commit/pass happy paths preserve exact ids; transaction rollback cannot return a value of another id kind; public signatures expose no raw n handle. Files: maki/cad.f, maki/pass.f, maki/store.f, transaction/pass tests. Verify: focused CAD/pass/store tests, maki/test.f, typed-local diff lint. Depends: CAD kind declarations.
