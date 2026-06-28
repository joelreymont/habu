---
title: Structured signature boundary
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:00:24.997696+02:00"
---

Problem: primitive rows are structural, but stack comments, TRUST, literal/numeric/control helper effects, and check-hook exports still enter src/core/checker.f as textual signatures parsed by PARSE-SIG. Fix: introduce a typed effect-construction API for internal/checker-owned effects and make TRUST/CHECK comments source-boundary adapters over canonical effect records, not general storage. Acceptance: internal primitive/literal/control/check-hook effect declarations use constructors/records; textual signature parsing is limited to user source comments and audited TRUST adapter input; FIND-SIG/DO-TOK never reparse stored effects; full native gate, dictionary/checker gate, trust-lint pass.
