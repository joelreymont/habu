---
title: "Add CURL:ERROR$ for libcurl's message"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T18:57:38.577358+02:00"
---

Logging lane 2. Brief: carl-dots/.out/carl-file/brief-log2-cerr.md. Acceptance: CURL:ERROR$ ( code -- ptr u8 n ) over curl_easy_strerror in src/lib/net/curl.f; curl-test asserts the texts for codes 28 and 1. Files: src/lib/net/curl.f src/lib/net/curl-test.f. Verify: bin/hb --load src/lib/net/curl-test.f on a rebuilt engine. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-cerr
