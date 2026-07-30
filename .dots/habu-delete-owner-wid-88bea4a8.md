---
title: Delete OWNER-WID persistence
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T06:09:57.350910+02:00"
blocks:
  - habu-retire-owner-wid-8552fe4f
---

Why: OWNER-WID has a dedicated writer, AOT and snapshot payload, loader validation, and build wiring even though production records zero rows. Result: in one hard cut remove the OWNER-WID emitter/seal module, AOT capture payload, snapshot and AOT reader/validator, trailer fields, build hooks, source-list rows, bootstrap wiring, and persistence-only tests or inventories. Writer and reader disappear together; rebuilt current artifacts contain no reserved bytes and old OWNER-WID artifacts are unsupported. Leave the in-memory registry and primitive/checker surface for the final deletion leaf. Owner: OWNER-WID persistence format and its direct build wiring only. Dependencies: owner-specific fixtures are retired. Production red: rebuilt images still reserve and validate an empty OWNER-WID payload. Acceptance: source, generated image, AOT, snapshot, build, and bootstrap inventories contain no OWNER-WID persistence symbol or bytes; current source rebuilds and reaches native fixpoint; PROT-WID persistence remains exact. Forbidden: version bump, migration reader, tombstone, padding reservation, replacement trailer, runtime registry deletion, lint, or compatibility. Smallest owning check: current source builds AOT and snapshot images that boot while old OWNER-WID trailer bytes are never parsed. Claim: unassigned.
