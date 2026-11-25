---
title: Ask for offline_access in BEARER's challenge and metadata
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T22:02:33.708242+02:00"
---

Problem: Entra gives ChatGPT a refresh token only when its sign-in asked for offline_access, so a server whose challenge and RFC 9728 document name only the API scope makes every user sign in again after an hour. Each server adds it itself: sapd src/serve.f, tenderd tenderd/mcp.f, Maki src/service.f, Zen through its config's scopes. tenderd, Zen and Maki also write their own RFC 9728 document instead of serving BEARER:RESOURCE-METADATA. mark reports Joel's decision (2026-11-25): BEARER asks for offline_access itself.

Acceptance: once METADATA! states a scope, each 401 challenge's scope and RESOURCE-METADATA's scopes_supported hold that scope and offline_access once, also when the caller's scope already names it; CURRENT's client-credentials requests and the check of a token's scp never name it. At the install, mark (sapd), kevin (Tender), phil (Zen) and tim (Maki) are told to drop their own offline_access and, where they have one, their own RFC 9728 document.

Depends: none. Claim: unassigned.
