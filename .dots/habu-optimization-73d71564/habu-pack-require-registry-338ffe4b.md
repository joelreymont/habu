---
title: Pack require registry path storage
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-25T11:22:29.358020+02:00\\\"\""
closed-at: "2026-09-25T14:30:45.867853+02:00"
close-reason: "Reviewed packed REQUIRE owner accepted: five identical native generations; all 490 native suites exit 0; Maki routing, geometry and negotiation pass with byte-identical board exports; strict macOS signatures pass. Native remains 2906359 bytes; Maki falls 508896 to 21193472 bytes. Unchanged warm recapture adds no DATA; mutating recapture retains old DATA copy. Exact receipts ~/.cache/tmp/habu-opt-round3/require-pool/RESULTS.md."
---

Replace the fixed 524800-byte REQUIRE path reservation with immutable DATA prefix and one bounded mapping; preserve canonical/provided facts and exact SLOT addresses for surviving rows until capture or discard. Reviewed contract ~/.cache/tmp/habu-opt-round3/require-contract-v2.md and independent require-design-review.md. Own src/core/include.f, obsolete storage comment src/habu/aot-ident.f, existing test/app-image.f E2E gaps declared before production edits. Failure modes: pointer movement on append, broken mapping mirror after truncate into frozen prefix, stale restored facts, unnecessary capture allocation after append/restore, incorrect map/unmap/allot failure publication. Acceptance: stable borrows, truncation/reuse and repeated saved-image facts through real E2Es, listed focused suites, native fixpoint/full gate, real Maki exports and measured native/warm bytes. Root owns review, integration, closure, push. No codec or general DATA reclamation.
