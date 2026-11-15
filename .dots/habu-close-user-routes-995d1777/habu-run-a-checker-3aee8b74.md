---
title: Run a checker scope as one word
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.044575+01:00"
closed-at: "2026-11-15T21:22:05.217414+01:00"
---

Problem: tools bracket a run with CHECKER-SCOPE-START-NEUTRAL and CHECKER-SCOPE-DONE (tools/check-core.f:1229-1233, check-all-errors-core.f:631-636, check-verify-child-lib.f:576-578); an unpaired DONE pops a frame its caller did not open. Acceptance: CHECK:SCOPED runs a quotation in a neutral scope and pops it on return or throw; the pair is private; the three tools call CHECK:SCOPED. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-store.
