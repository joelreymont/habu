---
title: Add WSTORE public sealed-table disposal
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T11:00:15.062862+02:00"
---

S3d amendment, discovered 2026-07-26 by the S6b1 checkpoint stop. WSTORE exposes no exit for a bare WSTORE:table: DISPOSE requires a store, and a store requires a mapping or a buffer, so any caller that seals a table and then fails before building a store leaks the table block with no public way to free it - a latent leak in landed S3d, and a hard blocker for the bind transaction, whose prep owns a sealed table and must be totally disposable (S6b1 ABORT, and the S6b2/S6b3 resource-exhaustion cleanup). The checker leaves no escape: dropping a WSTORE:table is rejected as non-certified. Behavior: public WSTORE:TABLE-DISPOSE ( WSTORE:table -- result<n,n> ) in maki/infer/weight-store.f, a thin public exit over the existing private table free path (TBL-FREE at weight-store.f:282), with the same result discipline DISPOSE uses. The package that mints a linear owner must own its exit. Tests: seal a table, dispose it, prove no leak via the existing WSTORE:LIVE counter; double-use rejected by linearity (checker negative); the DETACH-MAPPING workaround shape (fabricating a store through the mapped constructor to reach disposal) documented in the test header as the hack this word exists to make unnecessary. Owner: package WSTORE. Dependencies: none. Acceptance: weight-store focused suite green including the new legs; typed-local and package diff lints on the diff; maki/test.f green. Claim: agent=s6b1 workspace=.jj-ws/habu-s6b1-prepare (first commit of the resumed S6b1 lane).
