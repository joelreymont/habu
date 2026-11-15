---
title: Give each MD reason its own code
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.152686+01:00"
closed-at: "2026-11-16T04:07:36.034475+01:00"
---

Problem: E-REJECTED is a generic code: MD-CODE$ defaults (MD-NO-LOOP, MD-LOOP-CARRIED) and return-stack records via DCODE RF-RETURN print it. Acceptance: each record prints a code naming its cause; E-REJECTED goes if nothing needs it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
