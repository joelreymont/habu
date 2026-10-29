---
title: http-test fails assert 281 intermittently
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:02:40.876017+02:00"
---

src/lib/net/http-test.f on master (public hb, master tree a91d2953): 1 of 3 runs rc 1 with 'F281 TFAIL assert 281 expected true got false'; every run logs 8 'http: request rN failed, throw' lines (-9990, -3900, -5702) even when it passes. Same rate on the T0 stack engine (jerry-char) and it reddened the full suite in jerry-t0f (RED: http kind=exit code=1). Runs were on a loaded host (several hb jobs). Reduce assert 281 and the request failures; a test that passes with failed requests may be masking them. Logs /tmp/jerry-http/m3.out (red), m1.out (green). Workspace: none yet.
