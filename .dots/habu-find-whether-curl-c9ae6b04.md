---
title: Find whether CURL reuses an HTTP/2 connection whose session failed on a span overflow
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:52:43.515412+02:00"
---

Problem: not verified. After a body outruns the caller's span over cleartext HTTP/2, nghttp2 fails the session (-902) and libcurl 8.7.1 logs 'Connection #0 ... left intact', so a later transfer on the same handle or multi may reuse a connection whose HTTP/2 session failed, and sibling streams on it probably end failed 56 (the curl review's inference from a verbose log of src/lib/net/curl-test.f's HTTP/2 span case). The test's server sends GOAWAY after DATA, which hides it there. Acceptance: a reproducer in curl-test.f's server without the GOAWAY shows what a second transfer and a concurrent sibling stream answer after an overflow; the cause is reported, and a fix dot follows if either fails. Files: src/lib/net/curl.f, src/lib/net/curl-test.f. Verify: hb --load src/lib/net/curl-test.f. Depends: none. Ownership: curl.f. Not examined: Linux, TLS, a newer libcurl. Claim: unassigned.
