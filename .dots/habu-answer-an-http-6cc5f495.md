---
title: Answer an HTTP error on an empty answer
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T13:44:53.442080+01:00"
---

Problem: src/lib/net/http-response.f PLAIN-ERROR drops the body but keeps the headers a handler wrote, then adds its own Content-Type; a handler that set one before HTTP:ERROR! sends two (tim, Maki, /tmp/tim-habu-http-error-headers.md). Acceptance: every error hook renders on an empty answer; an http-test case shows one Content-Type. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-http-err.
