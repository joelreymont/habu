---
title: Frame bounded HTTP completions
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:07:41.469622+02:00"
blocks:
  - habu-own-checked-ipv4-44f29beb
---

Why: the server needs one narrow HTTP/1.1 product protocol. Interface: package HTTP-COMP owns caller-storage parsing and response framing for exactly POST /v1/completions HTTP/1.1. A request requires one Host and decimal Content-Length, forbids Transfer-Encoding, accepts only Connection: close, bounds request line, headers, count, and body before copying, and reports complete only after exactly Content-Length bytes across arbitrary fragmentation. A response writes status, application/json, Content-Length, Connection: close, and the exact body through a resumable partial-write cursor. Every value is connection-owned and reusable only after completion. Add no streaming, chunking, keep-alive, pipelining, other method or path, generic header map, global buffer, allocation, timeout, socket ownership, JSON codec, compression, TLS, WebSocket, HTTP/2, version, or compatibility mode. Owner: new maki/serve/http-completions.f only. Production red: fragmented socket bytes cannot become one bounded request or response. Acceptance: every split point and short write produces the same result; duplicate or conflicting length, Transfer-Encoding, missing Host, excess bounds, malformed CRLF, extra body, early EOF, and unsupported method or path reject without overread; exact response bytes match fixtures. Smallest owning check: focused framing tests plus one real loopback partial-I/O trace. Claim: unassigned.
