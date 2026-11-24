---
title: "Delete CURL:CA-PATH!"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T20:13:56.596021+01:00"
---

Problem: CURL:CA-PATH! (src/lib/net/curl.f:554) is reached only by curl-test.f:3277; no consumer or Habu package calls it. Acceptance: the word, its four option constants, its test and its header-comment mentions are gone; curl-test passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-capath.
