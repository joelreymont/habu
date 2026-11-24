---
title: Type an integer a cell holds in JSON Schema
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T16:52:28.830611+01:00"
closed-at: "2026-11-24T17:01:35.000372+01:00"
---

Problem: since 35e4940c NUMBER-TYPES (src/lib/json-schema.f:834) types any whole number as integer (JSON:INTEGRAL?), so a tool argument past a signed cell (an id of 1e20) passes VALID? and reaches the tool, whose JSON:>N cannot read it (phil, Zen, 2026-11-24). RFC 8259 section 6 lets a reader limit a number's range, and Habu reads an integer into a cell. Acceptance: type integer is a whole number a signed cell holds, any spelling (1.0, 1e2); a larger whole number fails with a fix naming the range; the json-schema suite passes. Depends: none. Claim: carl workspace=.jj-ws.noindex/carl-jsint.
