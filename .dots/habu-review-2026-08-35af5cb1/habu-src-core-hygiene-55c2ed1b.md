---
title: "src/core hygiene: latches, heuristics, JSON, magic numbers"
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.879667+02:00"
---

Problem: (a) checker.f:11102-11105 TRUST-RAW leaves SIG-RAW-MODE latched on if TRUST-USIG! throws; (b) src/core/top-row.f:50-51,222-236 detects '0 set-check' by token adjacency (FALSE set-check or MY-OFF set-check not seen) where the engine knows the installed hook; (c) render.f:389-408 JNUM renders negatives as empty and JCHAR escapes only five bytes, so control bytes in definition_source make invalid JSON; (d) decimal ASCII literals throughout checker.f (2876-2878, 3061-3063, 12487-12511), render.f, sumtype.f, type-family.f against docs/forth.md:955-958; exit code 76 appears 191 times unnamed; checker.f:8027 throws $4E while layout-buffer.f:28 names it E-DUP-DEFINITION; (e) stale comments checker.f:187-189, 2938, habu2.f:7725-7731, util.f:1-2, render.f:1-5, sha256.f:1-3,202, sha-check.f:3; (f) habu2.f:2668-2684 runs the check hook on definer bodies whose verdict is ignored and whose row PUBLISH-* overwrites (758 shadowed rows per checker.f:5452-5460). Acceptance: each item fixed or refuted with a line. Files: as listed. Verify: owning suites, error-code lint. Depends: none. Ownership: src/core. Claim: unassigned.
