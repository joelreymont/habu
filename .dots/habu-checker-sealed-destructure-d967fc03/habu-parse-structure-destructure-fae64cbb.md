---
title: Parse structure destructure policy
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:58.437906+02:00"
blocks:
  - habu-store-structure-destructure-8c20c92a
---

Problem: STRUCTURE declarations cannot select the registry destructure policy. Required syntax: an optional header clause DESTRUCT public|owner before the first FIELD; omission means public. The clause may appear once. owner requires a real non-global declaring package and a structure with at least one field; global, duplicate, missing, unknown, late, zero-field, and mixed-legacy forms reject at the offending token and roll the entire declaration back. Live parsing, DECL-EVENT, replay, verify-source, diagnostics, and docs/type-families.md grammar must carry the same code; substring scans or a replay-only side table are forbidden. No generation or call behavior changes in this leaf. Owner: STRUCTURE front end and declaration event/replay parity. Dependency: habu-store-structure-destructure-8c20c92a. Acceptance: structure suite hostile fixtures cover comments, strings, duplicate clauses, reordering, wrong package role, replay parity, exact diagnostics, and byte-identical rollback through real bin/hb declaration paths.
