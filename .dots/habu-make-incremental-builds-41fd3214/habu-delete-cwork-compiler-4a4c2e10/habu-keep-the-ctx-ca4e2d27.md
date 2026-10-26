---
title: Keep the context registry in DATA tables
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:38.581918+02:00"
closed-at: "2026-10-26T17:34:44.886037+02:00"
close-reason: superseded by the arena redesign (Joel approved 2026-10-26, ~/.cache/tmp/carl-file/wstate-recommend.md)
---

CWORK lane 1: IR-CTX registry (context.f HANDLES/ENTERED/BASES) becomes pointer-free DATA tables; BASES stores header offsets resolved with REGION-AT; drop require workspace.f in context.f. Brief ~/.cache/tmp/carl-file/brief-cw1.md. Workspace: .jj-ws.noindex/carl-cw1. Claim: agent=carl worker.
