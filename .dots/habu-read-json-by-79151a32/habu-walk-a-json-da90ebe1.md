---
title: Walk a JSON value in one pass
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.393283+01:00"
closed-at: "2026-11-24T19:40:34.176632+01:00"
---

Problem: ODATA:COPY-ENTITY and JSON-SCHEMA:VALID? descend to a peer-chosen depth and need a walk with no call per level (EACH nests one) and linear in the text (FIRST/NEXT over values is quadratic in depth). Acceptance: JSON:WALK and ENUM JSON:event (open-object, open-array, name, scalar, close) in src/lib/json/value.f with tests (a 100000-deep walk); JSON:NUMBER-COMPARE's row says str str. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jfirst.
