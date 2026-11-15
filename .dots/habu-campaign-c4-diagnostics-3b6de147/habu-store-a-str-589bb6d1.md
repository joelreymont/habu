---
title: Store a string literal of any length
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.056177+01:00"
closed-at: "2026-11-16T04:07:33.903798+01:00"
---

Problem: src/habu/intern.f ADD rejects a body over one 512 KB segment with E-NSTR-CAP, an uncaught throw (rc 67) that --verify-only accepts. Acceptance: such a body gets its own segment; E-NSTR-CAP deleted; a 600 KB literal runs. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
