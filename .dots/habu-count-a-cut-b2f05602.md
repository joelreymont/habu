---
title: Count a cut TLS handshake as lost
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T17:57:13.379945+02:00"
---

Problem: src/lib/net/curl.f:58-63 LOST? leaves out CURLE_SSL_CONNECT_ERROR (35), which libcurl answers when a peer resets or closes the connection during the TLS handshake, so a client that retries what LOST? accepts gives up on a transient cut. A failed handshake sent no request, so a retry is always safe; lookup (6) is already lost though it too can be permanent. Acceptance: a handshake the peer cuts answers 35, and LOST? accepts it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-count-a-cut-b2f05602.
