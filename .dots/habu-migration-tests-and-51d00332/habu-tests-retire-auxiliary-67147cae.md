---
title: "Tests: retire auxiliary enum fixtures"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:03.146351+02:00"
blocks:
  - habu-enum-expose-named-5bfe8bb0
---

Own test/gate-dictionary-lib.f ENUM+/ENUM4+ fixtures and remaining payloadless legacy enum helpers. Replace behavior coverage with compact ENUM and pinned removed-token rejection; prove runtime lookup cannot find auxiliary definers after deletion.
