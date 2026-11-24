---
title: "Move JSON readers onto JSON:value"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T18:02:40.228483+01:00"
---

Problem: Joel 2026-11-19: "json users should not have to deal with pointers, cursors, etc." and "no agent should be using "^u8 n" anymore! it's either str or vector". Libraries and programs still read JSON through the public token reader (src/lib/json/read.f: JSON:reader, JSON:cursor, READ-*, T-*, KIND$, POINTER-SPAN), and JSON-RPC, JSON-SCHEMA, ODATA, MCP, HTTP and JSONL take or answer cursors or JSON text.

Plan (settled by the long-term rule, 2026-11-24): package JSON owns all reading; read.f's cursor becomes private to value.f, copy.f, equal.f and pointer.f. Bytes go through JSON:PARSE once and libraries take and answer JSON:value. A word whose contract maps a parse failure to a protocol answer takes str (JSON-RPC:>MESSAGE). Walkers of peer-chosen depth (ODATA:COPY-ENTITY, JSON-SCHEMA:VALID?) keep their own frame stack and take one event at a time from JSON:WALK, which reads the value in one pass (a FIRST/NEXT walk over values costs depth squared, measured 2026-11-24: 32000 deep 44.7 s). A body that repeats a member anywhere is INVALID-REQUEST with the null id (one parse, unambiguous GET). Not built: WITH-READER (keeps the token reader public), JSON-SCHEMA:VALIDATE (no caller), a PARSE depth limit (RFC 8259 sets none), float or u64 readers (no program calls them: TEXT$ then STR>FLOAT). Lanes are this dot's children, prerequisites first; consumers' agents move their own code, and each install that removes a word sends sapd the notice.

Acceptance: `rg -i --no-ignore 'JSON:(reader|cursor|T-[A-Z]+|READ-[A-Z0-9$?-]+|POINTER-SPAN|SEEN-BYTES|KIND\$)' src tools test` finds nothing; no public row in json-rpc.f, json-schema.f, odata/response.f, mcp.f, jsonl.f or http-request.f names a cursor, a reader or `^u8 n` for JSON; VALID? and COPY-ENTITY pass 100000-deep tests. Depends: the children. Claim: agent=carl (lead).
