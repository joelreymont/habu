---
title: Add RFC 6455 WebSocket server support over lib/net/http.f
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T13:39:54.079864+02:00"
blocks:
  - habu-move-tender-s-b5dbc521
---

Problem: Maki streams meshes to a browser viewer and Habu has no WebSocket (maki docs/habu-needs.md). The handshake needs SHA-1 and base64: lib/crypto/evp.f:67,:333 has only HMAC-SHA-1 through libcrypto, the engine digest is SHA-256 (src/core/sha256.f), lib/ has no base64. The HTTP worker serves a connection request by request (tender server/http.f:243-256) and cannot give it to an upgrade handler. Design: lib/net/ws.f, package WS: `WS:ACCEPT ( HTTP:request HTTP:response -- WS:socket )` inside a route handler validates Upgrade, Connection, Sec-WebSocket-Version 13 and Key, answers 101 with Sec-WebSocket-Accept, and the handler owns the socket until it returns; `WS:RECEIVE ( WS:socket ms -- WS:message )` with text, binary, closed and timeout arms, reassembling continuations, answering ping, closing 1002 on an unmasked or oversize control frame and 1007 on invalid UTF-8 (lib/utf8-scalar.f); `WS:SEND-TEXT`, `WS:SEND-BINARY`, `WS:CLOSE ( WS:socket n -- )`, sends serialised by a TASK:SEMAPHORE per socket so a push from another task interleaves whole frames. The frame codec (lib/net/ws-frame.f), SHA-1 (lib/crypto/sha1.f) and base64 (lib/base64.f) need no HTTP and land first. Decided: pure-Habu SHA-1 and base64, no libcrypto in the server path; the upgraded socket lives in its HTTP worker for its life for the first landing. Acceptance: lib/net/ws-test.f: a Habu client over TCP4 handshakes (RFC 6455 section 1.3 key answers s3pPLMBiTxaQ9kYGzzhZRbK+xOo=), echoes text, a 70000-byte binary (16- and 64-bit lengths), a fragmented message, ping/pong, each close, a close handshake, a push from a second task; artifact build/ws-transcript.txt. Files: those, lib/errors.f block -9310, docs/websocket.md, test/gate-stdlib-cases.f. Verify: bin/hb --load lib/net/ws-test.f; SUITE websocket in test/run.f. Depends: habu-move-tender-s-b5dbc521 (ws.f and the suite only). Ownership: lib/net/ws*.f, lib/crypto/sha1.f, lib/base64.f, docs/websocket.md. Claim: unassigned.
