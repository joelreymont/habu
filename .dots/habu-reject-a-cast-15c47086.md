---
title: Reject a cast out of another scratch family
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.772829+01:00"
---

Problem: src/lib/aio-test.f:612-613 casts out of AIO's SCRATCH-ONLY family from another package and is admitted, so a scratch value can reach a saved cell as `n` (docs/forth.md lets projections out need no private section). Acceptance: a CAST: whose source is another package's SCRATCH-ONLY family is rejected, with a help line; the family's own package may project. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cscr.
