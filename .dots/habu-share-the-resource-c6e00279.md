---
title: Tell the servers BEARER names offline_access
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T22:02:33.708242+02:00"
---

Problem: BEARER names offline_access itself in each 401 challenge and in RESOURCE-METADATA's scopes_supported. sapd (src/serve.f), tenderd (tenderd/mcp.f), Maki (src/service.f) and Zen (its config's scopes) still add it themselves, and tenderd, Zen and Maki write their own RFC 9728 document.

Acceptance: at the next install mark (sapd), kevin (Tender), phil (Zen) and tim (Maki) are told to drop their own offline_access and, where they have one, their own RFC 9728 document.

Depends: the next install. Claim: agent=jerry.
