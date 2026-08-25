---
title: Package Maki stores
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T22:04:05.561057+02:00"
---

Current master package-first violation and bloat: maki/store.f opens the broad MAKI package, then recreates subsystem scope with STORE-*, CLS-*, SCHED-*, MEAS-*, EVID-*, PROFIT-*, and CALIB-* global tails. maki/store-replay.f similarly adds STORE-REPLAY-*/REPLAY-* inside MAKI, while maki/sched-key.f owns SK-* in the same megapackage. The result is redundant external names such as MAKI:STORE-..., unrelated persistence authority under one package, collision pressure, long dictionary names, and no package-enforced boundary between transport, replay lifecycle, schema queries, and schedule-key state. Create real STORE for transport/row framing, STORE-REPLAY for lifecycle and durable publication, and SCHED-KEY for the key table; give other schema families an owner only when they cross a real responsibility seam. Use short private tails and expose only narrow qualified APIs; remove every pseudo-namespace prefix and compatibility alias. Preserve wire bytes, public behavior, durable paths, replay ordering/errors, query results, schedule-key identity, and Maki load order. Prove old prefixed spellings and cross-package private access reject; exact callers use qualified APIs; package/private and bare-load gates cover each module; dictionary-name bytes, definition count, loaded JIT/DATA/CODELEN, and build time are measured before/after with no unexplained growth. Serialize shared edits with habu-type-maki-store-57537d27, habu-structure-store-query-63edd08e, habu-type-store-replay-634e025b, habu-make-store-replay-7cd1f6d7, and habu-factor-maki-store-24dc8f8b so representation, transactionality, factoring, and naming change once.
