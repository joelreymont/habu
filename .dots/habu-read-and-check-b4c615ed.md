---
title: Read and check JSON Schemas without recursion
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T08:39:04.296966+01:00"
---

Problem: JSON-SCHEMA:COMPILE reads each sub-schema by a nested call (src/lib/json-schema.f SUB, READ-SCHEMA, OBJECT through JSON:READ-MEMBERS's handler), so a schema nested about 50000 deep ends the process (exit 102, E-STACK-BOUNDS) where no catch gets it; VALID? descends the same way (CHECK, MEMBER, ELEMENT-OK?, ANY?, PASSING) and reads each nested value once per enclosing level (READ-ANY-SPAN$ reads through it, then CHECK reads it again). Acceptance: a schema and a value each nested 100000 deep compile and check, answering instead of ending the process; time linear in the text; every existing json-schema and mcp test answers as before. Depends: the MCP line lands first. Claim: agent=carl workspace=.jj-ws.noindex/carl-mcl5 (design).
