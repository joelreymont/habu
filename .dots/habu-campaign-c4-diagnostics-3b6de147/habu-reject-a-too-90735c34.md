---
title: Reject a too-big literal as a literal
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.133508+01:00"
closed-at: "2026-11-16T04:07:34.474593+01:00"
---

Problem: a number too big for a cell is rejected E-UNDEFINED with the help 'define 999... before this point' (checker.f LITERAL-TOK?, ~:17173). Acceptance: its own record names the literal and its range, with a help to fix it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
