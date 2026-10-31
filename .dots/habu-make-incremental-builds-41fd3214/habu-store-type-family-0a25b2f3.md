---
title: "Store type-family.f's object in no-diag cold builds"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T09:59:38.340311+02:00"
---

After the hook fix (pxpzvlxw), a cold engine build without HABU_NATIVE_CACHE_DIAG stores 146 objects; type-family.f's is silently refused (stored under diag, and at the parent). Root-cause and fix. Brief carl-dots/.out/carl-file/brief-tfam.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-tfam.
