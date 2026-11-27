---
title: Reach a loopback IP literal without a proxy
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:40:22.560045+01:00"
---

Problem: URI:SECURE? admits http to a loopback IP literal as secure because the request stays on the host, but CURL leaves routing to libcurl (src/lib/net/curl.f:3), which sends such a request through the proxy http_proxy or all_proxy names: with http_proxy=http://127.0.0.2:9 a POST to http://127.0.0.1:N/mcp carrying 'Authorization: Bearer' reached the listener on 127.0.0.2 in clear text (libcurl 8.5.0). MCP's exchanges (src/lib/net/mcp.f PREPARE), BEARER's token POST (src/lib/net/bearer.f REQUESTED?) and the Azure vault fetch (src/lib/secret/azure.f AZURE-ASK) all rely on the exemption. Acceptance: a CURL request to a URL whose host is a loopback IP literal never uses a proxy, from the environment or PROXY!; any other URL keeps libcurl's proxy rules; curl-test pins both. Depends: none. Claim: unassigned.
