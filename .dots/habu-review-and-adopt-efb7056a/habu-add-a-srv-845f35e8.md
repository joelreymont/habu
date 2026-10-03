---
title: Add a server-durable intent profile to SYNC
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.966088+03:00"
---

Problem: HBR2 §18.3 journals each operation in IndexedDB before sending it, and §22.2 makes editing depend on browser storage policy and adds EncryptedStorage and server key wrapping; Maki is online only and its intent is durable at the server receipt (maki docs/viewer.md); §22.2 names a server-only profile as needing its own contract. Acceptance: a SYNC profile with no Journaled state (Constructed, VolatileProjected, Sent, AcceptedAwaitingEvent, Applied, Rejected); receipts in memory for the horizon; in-page retries reuse the OperationId; a page death loses only unacknowledged operations, which the UI showed as Unsaved; boot resubscribes at the server head with an empty SyncHello.unresolved; §14.4 AwaitingDurability, §19.5 journal partitions and §22.2's StoragePolicyDenied fallback do not apply; the profile's crash matrix is checked against the reference model. Files: lib/sync/, docs/browser-runtime.md. Verify: W-SYNC and W-RECOVERY variants for the profile; the T01-T24 subset that touches durability. Depends: habu-build-hbr2-runtime-731d5ddd. Ownership: lib/sync/. Lane: tim. Claim: unassigned.
