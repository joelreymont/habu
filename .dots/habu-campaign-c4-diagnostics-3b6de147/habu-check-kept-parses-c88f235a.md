---
title: "Check KEPT-PARSES without TRUSTED:"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.559812+01:00"
closed-at: "2026-11-16T04:07:36.590348+01:00"
---

Problem: C4 added a TRUSTED: body, KEPT-PARSES (verify-source.f:1967-1968, called at :3164); PROW-FIND has no checked effect (check-hook.f:103 calls it directly). TRUSTED: is being retired. Acceptance: KEPT-PARSES is checked; no TRUSTED: is added. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
