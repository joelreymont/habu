---
title: Compile a JSON Schema from a value
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.416893+01:00"
closed-at: "2026-11-24T19:40:34.914451+01:00"
---

Problem: JSON-SCHEMA:COMPILE takes the schema's text and reads it with a cursor. Acceptance: COMPILE ( JSON:value ^ARENA:arena -- schema ); mcp.f:1096 follows. Depends: one-pass walk lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-jschema.
