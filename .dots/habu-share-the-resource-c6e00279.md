---
title: Share the resource-server scope check and RFC 9728 document in BEARER
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T22:02:33.708242+02:00"
---

Maki's sign-in (ws r2-signin, src/projects.f 155-213) repeats tenderd/mcp.f's resource-server words almost line for line: GRANTS-AT/SCOPED? check that an admitted token's space-separated scp holds one scope token, and RESOURCE-BODY builds the RFC 9728 protected-resource metadata document; KEY+/CLAIM+ build an app-side owner key and stay in the apps. Two working callers make the scope-token check and the RFC 9728 document candidates for src/lib/net/bearer.f. New public words: names and shapes go to Joel before a lane. Proposed by tim (Maki), 2026-11-01. Owner: carl.


Not dispatchable (audit 2026-11-10): a proposal with no acceptance. It needs the words, their effects and the two callers that move (tenderd/mcp.f, Maki projects.f).
