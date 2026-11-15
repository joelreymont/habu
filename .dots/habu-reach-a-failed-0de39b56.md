---
title: "Reach a failed stream's header publish every run"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T17:57:13.390492+02:00"
---

Problem: src/lib/net/curl.f STREAM-FINISH publishes the final header block of a stream that failed after body bytes (u > 0); CASE-STREAM-PARTIAL in src/lib/net/curl-test.f reaches that branch only when one curl_multi_perform reads both the body bytes and the close, since TRANSFERS-RUN reaps before SERVICE-RECORDS; otherwise HEADERS-PUBLISH answers, so a regression in the branch can pass. Acceptance: a test reaches that branch on every run and fails when the branch publishes nothing. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-reach-a-failed-0de39b56.
