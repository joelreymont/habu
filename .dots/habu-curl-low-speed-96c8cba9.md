---
title: "CURL:LOW-SPEED! also bounds the connect phase"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.956113+02:00"
---

From phil (Zen), 2026-11-05. Problem: src/lib/net/curl.f:619 LOW-SPEED! sets CURLOPT_LOW_SPEED_LIMIT/TIME, which libcurl applies only once the transfer runs; a connect that never completes waits libcurl's default 300 s (measured by phil), so a stall test a caller set does not cover the one phase where nothing at all arrives. Acceptance (lead's call, no new word): LOW-SPEED! also sets CURLOPT_CONNECTTIMEOUT_MS (156) to its seconds window, and zero removes it as it removes the stall test; its comment says the connect phase is covered; a test against a listener that accepts nothing (or a blackholed address) ends within the window. Files: src/lib/net/curl.f, src/lib/net/curl-test.f. Verify: bin/hb --load src/lib/net/curl-test.f. Depends: none. Ownership: carl. Claim: unassigned.
