---
title: Library storage handed to callers as a byte size becomes a typed record
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T08:06:18.025080+02:00"
---

Problem: libraries make callers allot storage by a byte size they export, against docs/design.md:19-21: src/lib/net/event-stream.f SSE:STORAGE-BYTES (callers: src/lib/net/mcp.f:1031, event-stream-test.f:10, Tender 5 SSE uses), src/lib/serial-xmodem.f SESSION-BYTES (:313); Tender writes STORAGE-BYTES 49 times, Maki 7, Zen 3. Acceptance: each such library exports a record type its callers hold in a typed container; no exported byte size for caller storage remains in src/lib. Depends: habu-buf-buf-a-48c31f14. Claim: unassigned.
