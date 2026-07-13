---
title: "Fields: retire value-record rows"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:54.349125+02:00"
blocks:
  - habu-fields-retire-product-3f934f5c
---

Own the VREC field/schema store in src/core/checker.f and focused checker tests. Redirect value-record field registration and lookup to the shared field arena, then remove duplicate VREC field/name/schema storage without changing candidate rollback or snapshot behavior. Validate engine, rollback, and snapshot slices.
