---
title: Release callback buffers when a declaration fails
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:21.880929+01:00"
closed-at: "2026-11-16T04:07:34.373309+01:00"
---

Problem: src/lib/ffi-callback.f REST is skipped when EMIT throws, leaving TOK-BUF and NAME mapped into a later image save. Acceptance: REST runs under finally; a caught failing declaration leaves both released. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
