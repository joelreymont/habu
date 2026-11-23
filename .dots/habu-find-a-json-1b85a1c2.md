---
title: "Cut the cost of JSON:PARSE's repeated-key check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T11:11:48.033730+01:00"
---

Problem: JSON:PARSE finds a repeated member by sorting each object's decoded keys (src/lib/json/read.f FIRST-REPEAT, KEY<): one 52 MB object of 4.3M keys takes about 11 s against 1.1 s for JSON-SCHEMA:VALID? (phil, /tmp/phil-json-parse-cost.md), and MCP's ADMITTED? will PARSE every tools/call body. Acceptance: PARSE of that object within a small factor of the schema check, with a bounded worst case for crafted keys; the earliest repeat and its place unchanged; whatever engine-boundary change the design needs (json.f is compiled into hb, map.f and entropy.f are not) is part of the change. Depends: the key-storage lane (carl-jkeys) lands first. Claim: agent=carl workspace=.jj-ws.noindex/carl-jhash (design running).
