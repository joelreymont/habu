---
title: Add maxima e2e operation gate
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-19T08:52:32.337870+01:00\\\"\""
closed-at: "2026-02-19T09:23:01.468510+01:00"
close-reason: Added deterministic e2e readiness status test in src/tests/integration.zig (commit 547b6a50). Baseline captures loaded operations and current failing operations for follow-up closure.
---

src/tests/integration.zig: add full-source maxima gate that loads full core list and checks operation smoke path for simplifya,diff,solve,integrate,factor,limit,determinant,expand,sin,cos with deterministic status vector; run under habu-stop-on-error and record missing binders. Depends on declaration leakage fix d0bf4864 and source-root wiring dots.
