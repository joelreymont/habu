---
title: "curl-http's halted-done case depends on winning a race"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:27:55.673809+01:00"
---

Problem: src/lib/net/curl-test.f CASE-HALTED-DONE (:2887) asserts 'a completed transfer is abandoned within the attempt bound' (DONE-ABANDONED 1 T=), which holds only if, in one of DONE-TRIES (8) attempts, the main task's TASK:HALT beats DONE-TASK's wake by the published response in AWAIT (src/lib/net/curl.f :1822). On spark it failed in 3 of about 80 runs of curl-http (attempts=8 abandoned=0; logs /tmp/carl-crca/top, /tmp/carl-crca/instload). Acceptance: every run exercises the halted-owner abandon of a completed transfer with no retry loop and no test-only seam in curl.f, and curl-http passes a repeat loop long enough to have shown the 1-in-27 failure. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-curldone
