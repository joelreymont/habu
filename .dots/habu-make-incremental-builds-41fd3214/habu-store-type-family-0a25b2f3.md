---
title: "Store type-family.f's object in no-diag cold builds"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T09:59:38.340311+02:00"
---

After the hook fix (pxpzvlxw), a cold engine build without HABU_NATIVE_CACHE_DIAG stores 146 objects; type-family.f's is silently refused (stored under diag, and at the parent). Root-cause and fix. Brief carl-dots/.out/carl-file/brief-tfam.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-tfam.

Resolved by lane 11's unmrmrst (stale process map): at the lane 11 tip type-family.f is refused every build (mapped-compile-time-read TF-A-P) in both modes, so no object is stored either way (dot habu-key-a-file-b9c53392). Closes at the lane 11 landing; reuse of the file is dot habu-reuse-checker-f-5bd58a27.
