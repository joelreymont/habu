---
title: "DIAG: promote revision + evidence fields to nominal ids"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-17T18:24:21.565122+02:00\""
---

Follow-up of habu-v2-structured-diagnostic-18d24536 (landed 6b19cda8): the diagnostic ADT's revision field is a string and invalidated-evidence is string[] because rev-id/evidence-id had no owner registries when the lane designed them - but maki/rev.f (package REV, rev-id COMMIT intern + ID>WIRE/WIRE>ID) landed mid-lane, so the revision promotion is NOW mechanical: serialize revision as CAD-KIND:rev-id via REV across the package boundary (wrong-width->malformed, unknown->bounds fold), update the four lossless-lowering fixtures + round-trip. The invalidated-evidence promotion waits on an evidence-id owner registry - that leg belongs to the evidence dots (habu-v2-evidence-applicability-73ac58b9 / habu-v2-evidence-promotion-f8312ebe should mint EVIDENCE per the plan-23.9 codec mechanism when they land; note added there). Also consider promoting parent-diagnostic from code n once diagnostics get durable identity (later dot). Files: maki/db/diagnostic.f, diagnostic-test.f. Ownership: V2 diagnostics.

Claim: unassigned (stale claim stripped 2026-08-04: the named workspace no longer exists on disk or in `jj workspace list`). Scope stays maki/db/diagnostic.f + test, revision->rev-id leg only.

REVISION LEG LANDED 2026-07-17 (diagpromo lane, commit 8aeebaf5; claim
RELEASED): revision is now an optional CAD-KIND:rev-id in a typed column,
serialized via REV:ID>WIRE / WIRE>ID with the standard reject fold,
rendered via REV:CONTENT$ in both renderers; all five fixtures mint real
rev-ids via REV:COMMIT; no new error codes. REMAINING on this dot: the
invalidated-evidence promotion (string[] -> evidence-id[]) once the
EVIDENCE owner registry lands with the evidence dots, and the
parent-diagnostic durable-identity consideration.

NOTE 2026-07-17 (keywire landing afd6fa0c): diagnostic.f serializes
producer/config/rev ids as process-local 8B raws - fine while bundles
are in-memory, but if diagnostics ever become durable/portable they need
the KEY>WIRE cross-process forms (surfaces now exist on all owners).
Fold that migration into this dot's next leg or mint one when
durability lands.
