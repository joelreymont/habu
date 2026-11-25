---
title: "Say str in the JSON reader's remaining rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.085773+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). src/lib/json/read.f KIND$, READ-SPAN$, READ-STR, READ-VALUE-SPAN$, READ-ANY-SPAN$, READ-MEMBER$, READ-REJECT, SEEN-BYTES, READ-LISTED, READ-REWIND, READ-REPEAT and tools/gate-json-assert-core.f GJA-STRING-INTO take or give text as ^u8 n. Acceptance: text in and out is str (a destination buffer a vector); callers move with it; consumer owners are told before the install. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-jsonstr.
