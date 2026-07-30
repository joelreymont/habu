---
title: Parse bounded completion HTTP
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T22:07:41.469622+02:00"
blocks:
---

Why: the server needs one narrow caller-storage request protocol. Interface: package HTTP-COMP defines REQUEST-BOUND ( n -- n ) and one parser for exactly POST /v1/completions HTTP/1.1. A request requires one Host and decimal Content-Length, forbids Transfer-Encoding, accepts only Connection: close, bounds request line, header bytes, header count, and body before copying, and reports complete only after exactly Content-Length bytes across arbitrary fragmentation. REQUEST-BOUND returns the checked fixed header capacity plus maximum body. Parser state is connection-owned and reusable only after one terminal result. Owner: request bound and request parser only in one request module. Production red: fragmented caller bytes cannot become one bounded completion body. Acceptance: every split point produces the same result; duplicate or conflicting length, Transfer-Encoding, missing Host, excess bounds, malformed CRLF, extra body, early EOF, unsupported method or path, and arithmetic overflow reject without overread. Forbidden: response bound, response cursor, socket call, streaming, chunking, keep-alive, pipelining, generic header map, global buffer, allocation, timeout, JSON, compression, TLS, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/http-request-test.f through the production parser. Claim: unassigned.
