---
title: CURL answers failed 56 for a body that outruns the span over HTTP/2
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:55:07.635383+02:00"
---

Problem: reported by tim 2026-11-10, not yet reproduced by carl: when a response body outruns the caller's span over HTTP/2, CURL answers 'failed 56' instead of 'truncated'. OVERFLOW? (src/lib/net/curl.f:460) requires libcurl code 23 (write error), and libcurl gives 56 (receive error) there. Reproducer facts and logs: /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/carl/curl-overflow/README.md. Nothing in Maki waits on it. Acceptance: a body larger than the span answers truncated over HTTP/2 as over HTTP/1.1; a test holds it. Files: src/lib/net/curl.f and its test. Verify: the reproducer in the README; the curl test. Depends: none. Ownership: curl.f. Hunt brief: carl-layout/.out/briefs/hunt-curl-overflow-h2.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-curl (hunt done; fix lane running, brief carl-layout/.out/briefs/fix-curl-overflow-h2.md)
