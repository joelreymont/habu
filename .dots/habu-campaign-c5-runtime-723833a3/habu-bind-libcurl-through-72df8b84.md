---
title: Bind libcurl through the FFI for HTTPS
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-16T13:54:54.720416+03:00\""
closed-at: "2026-09-17T03:44:25.324564+03:00"
close-reason: "implemented, reviewed, landed on hazel's line and integrated at d7555528, gate R green (417 of 417)"
---

Problem: a server must scrape platforms, call Entra ID, Microsoft Graph and LLM APIs over HTTPS, and Habu has no TLS or HTTP client; VFX Forth solves this by binding libcurl rather than writing a client (docs/socket-models.md). Acceptance: package CURL over the existing FFI (dlopen of libcurl.so.4) with typed handles and a result ADT: init, set URL, method, headers, body, cookie jar path, timeout; perform into a caller-owned byte buffer with status code; cleanup; failures carry the curl code and never leave a handle; a loopback test against a local HTTP server started in a task, and one opt-in HTTPS test against a public endpoint. Files: lib/net/curl.f (new), lib/net/curl-test.f, docs/curl.md. Verify: the tests; test/run.f green. Depends: habu-add-tcp-sockets-fb1d351e (the loopback server). Ownership: runtime lane. Claim: unassigned.
