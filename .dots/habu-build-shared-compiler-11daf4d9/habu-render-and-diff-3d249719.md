---
title: Render and diff compiler IR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.439866+02:00\""
blocks:
  - habu-canonicalize-compiler-tables-e0c7f8f1
---

Full context: design sections 5.6 and 6.6 require deterministic diagnostic rendering and structural diff that are never parsed by compiler code. Render every frozen table/reference with stable names and source locations; diff semantic structure, not text. Acceptance: golden output is deterministic and read-only; a repository search/gate rejects compiler parsing of renderer output. Dependency: canonical tables.

Claim: agent=irrender workspace=.jj-ws/habu-render-and-diff-3d249719
