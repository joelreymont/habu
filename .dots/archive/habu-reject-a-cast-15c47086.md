---
title: "Reject a cast naming another package's scratch family"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.772829+01:00"
closed-at: "2026-11-19T14:57:21.531009+01:00"
---

Problem: src/lib/aio-test.f:612-613 casts out of AIO's SCRATCH-ONLY family from another package and is admitted, so a scratch value can reach a saved cell as `n` (docs/forth.md lets projections out need no private section). Acceptance: a CAST: outside a SCRATCH-ONLY family's package that names the family on either side, a view's element included and a phantom argument aside, is rejected, with a help line; the family's own package casts freely. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cscr.
