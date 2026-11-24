---
title: "Compile SPARQL to PostgreSQL for Zen's query tool"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T21:32:20.084615+01:00"
---

Problem: Joel 2026-11-24: replace Zen's ad hoc SQL interface to ChatGPT with SPARQL; make it rock-solid, efficient, tightly written and working. Design /tmp/carl-sparql/design.md. Acceptance: SPARQL:COMPILE compiles the W3C query cases (src/lib/sparql test files pass), the design's Evidence bounds hold, Zen's query tool answers through it, one Fable review of the combined diff passes, landed and installed. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-sparql.
