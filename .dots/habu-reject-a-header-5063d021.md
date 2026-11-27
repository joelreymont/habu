---
title: Reject a header value holding CR, LF or NUL
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T15:09:15.120900+01:00"
---

Problem: src/lib/net/http-request.f PARSE-HEADER (:375) splits the head only at CRLF (LINE-END :327) and never checks a value's bytes, so a bare CR, a bare LF or a NUL inside a header value is accepted and handed to the application; RFC 9110 5.5 requires such a message be rejected or the bytes replaced with SP, and a proxy that ends a line at a bare LF reads a different request (smuggling). Acceptance: a request whose header value holds CR, LF or NUL is answered 400, with a test. Depends: none. Claim: unassigned.
