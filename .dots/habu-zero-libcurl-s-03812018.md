---
title: "Zero libcurl's freed copies of credentials"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T14:54:55.889293+01:00"
---

Problem: libcurl 8.5.0 frees its copies of what Habu sends with a plain free: the CURLOPT_COPYPOSTFIELDS body (lib/setopt.c:480-524, freed via Curl_setstropt and Curl_freeset, url.c:178), the request dynbuf that holds a body under 64 KiB and every header line (http.c:2715-2734, dynbuf.c:57-60), the TLS upload buffer (http.c:1366-1390, freed multi.c:698, url.c:285), and strdup copies of URL, proxy and userinfo (url.c:1897, 1918). So BEARER's token form (client_secret), and API keys and bearer tokens model.f and mcp.f send through HEADER+, stay in freed heap after the transfer; switching CURL:BODY! to CURLOPT_POSTFIELDS removes only one copy. src/lib/net/curl.f:247-250 promises zeroing only for buffers curl.f frees. Acceptance: after a transfer that sends a secret through package CURL, no heap block libcurl freed still holds it, shown by a test; the curl.f guarantee comment states what is covered. Depends: none. Claim: unassigned.
