---
title: Delete the dead strict-boundary switch
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.707356+01:00"
closed-at: "2026-11-16T04:07:36.359302+01:00"
---

Problem: after PRIVA deleted the set-check lint nothing reads tools/checked-boundary-lint-core.f STRICT! / UB-STRICT-BOUNDARY, argv.f's --strict-boundary flag or tools/hook-sites.f CHECK-MATCH? and check-hook.f's DEFINITION row. Acceptance: deleted; --strict-boundary is rejected as an unknown flag; argv passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
