---
title: Decode a JSON string into a byte buffer
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:20:18.736282+02:00"
---

Problem: JSON reads the reader's current string only into a caller's span (READ-STR), so a caller wanting it in a BUF byte buffer (src/lib/byte-buffer.f) repeats four steps: READ-SPAN$ for the raw length as an upper bound, BUF:GROW by it, READ-STR into the grown span, cut the buffer back to the decoded length. Requested by Maki (tim, 2026-11-10; request file /tmp/tim-json-str-buf.md, copied here in full): Maki holds two copies, /Users/joel/Work/maki/src/project.f:302-307 (PROJECT's private STR-APPEND, six callers in src/project*.f; STR-SET :311 clears first) and deploy/release/rebuild.f MEMBER$ (lane reg-rebuild), which cannot reach PROJECT's private word. Acceptance: one public JSON word that appends the reader's current string, decoded, to a byte buffer and leaves the reader after it as READ-STR does; an empty string appends nothing; the buffer's earlier bytes and saved offsets stay valid as BUF's appends keep them. Its name and stack effect are a new public word a user program calls: Joel names it before a worker starts. Files: src/lib/json.f, its test. Verify: a json test case for a string with escapes, an empty string and an append after earlier bytes. Depends: none. Not urgent: nothing in Maki waits on it. Ownership: unassigned. Claim: unassigned.

Named by Joel (2026-11-10, interview answer, his chosen option): JSON:READ-STR-APPEND, effect ( JSON:reader ptr a -- JSON:reader ) as the question stated it. src/lib/json/read.f gains a require of src/lib/byte-buffer.f. Ready for a worker brief; nothing waits on it.

Consumers: Maki's two private copies are now src/project.f:304-309 STR-APPEND and deploy/release/rebuild.f:231-239 MEMBER$.

Named by Joel (2026-11-11, interview answer, his chosen option): the BUF word the append needs to cut the buffer back is BUF:LEN! ( NUM:byte-len ptr a -- ), VEC:LEN!'s contract (src/lib/vector.f LEN!): any length from zero through the capacity, above it E-BUF-BOUNDS; the capacity stays. The other options were a shorten-only BUF:TRUNCATE or BUF:SHORTEN and no new word (BUF:REPLACE on the buffer's own bytes).

Claim: agent=carl workspace=.jj-ws.noindex/carl-json
