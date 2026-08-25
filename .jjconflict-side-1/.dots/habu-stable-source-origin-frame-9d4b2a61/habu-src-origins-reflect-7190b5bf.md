---
title: "Source origins: reflect diagnostics"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:52.008869+02:00"
blocks:
  - habu-src-origins-intern-1dd60d2e
---

Problem: semantic origin identity and human diagnostic coordinates are currently conflated, risking path-dependent type hashes. Acceptance: attach immutable diagnostic path, include chain, line, column, and exact token span to a canonical origin and expose read-only reflection. None of these diagnostic coordinates, the checkout root, raw handles, or allocation order may enter family, field, constructor, layout, or artifact semantic hashes. Moving identical authenticated source changes diagnostics only. Files: source-origin diagnostic reflection, render integration, and JSON/text fixtures. Verify: same content under two roots, nested include/evaluate spans, line/column boundaries, escaped paths, and unchanged semantic hashes. Depends: Source origins: intern declaration spans. Ownership: diagnostic-coordinate reflection and rendering only. Claim: unassigned.
