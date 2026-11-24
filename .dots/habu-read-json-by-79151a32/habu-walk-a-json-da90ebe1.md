---
title: Walk a JSON value by first and next
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.393283+01:00"
---

Problem: ODATA:COPY-ENTITY and JSON-SCHEMA:VALID? descend to a peer-chosen depth and need the first value inside a container and the value after one, which EACH (a call per level) cannot give. Acceptance: JSON:FIRST and JSON:NEXT in src/lib/json/value.f with tests (a 100000-deep walk); JSON:NUMBER-COMPARE's row says str str. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jfirst.
