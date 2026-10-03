---
title: Provide the server half of SYNC
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.979040+03:00"
---

Problem: HBR2 specifies client SYNC and only a server fixture (§27.3 G2); the server side (gap-free per-document order within an epoch, dedup by tenant/document/OperationId with a body-hash check, receipts within a negotiated horizon, OrderedEvent publication, checkpoints, SyncHello/SyncWelcome reconnect, presence; §18.2-18.5, A.2) is generic to any collaborative HBR2 application; Habu has lib/net/http.f and lib/net/ws.f. Maki is the first caller; deciding what accepts an operation is the application's. Acceptance: a library over lib/net/ws.f that an application configures with an acceptance callback (operation to accepted event or rejection) and an epoch source; the G2 server fixture built on it; a W-SYNC two-client test with receipt reorder and an epoch bump. Files: lib/sync/server*.f, docs/browser-runtime.md. Verify: §18.2-18.5 ordering and dedup properties through the real socket. Depends: habu-build-hbr2-runtime-731d5ddd. Ownership: lib/sync/server*.f. Lane: tim. Claim: unassigned.
