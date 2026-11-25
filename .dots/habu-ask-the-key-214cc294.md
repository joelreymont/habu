---
title: Ask the Key Vault once per item
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T11:31:43.553379+01:00"
---

Problem: under SECRET:AZURE-BIND each SECRET:READ is a Key Vault GET (src/lib/secret/azure.f AZURE-READ), and MODEL reads its key at every call (src/lib/net/model.f KEY-READ), so every model call costs a vault request, fails while the vault is unreachable and writes an audit record (/tmp/joel-bb-habu-kv-per-call.md). Acceptance: a found item is fetched once per process and served from memory after; a miss or failure is asked again; a bind forgets held items; secret-azure passes with a case that fails on today's azure.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-kv.
