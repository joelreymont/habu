---
title: "DIAG: promote revision + evidence fields to nominal ids"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T18:24:21.565122+02:00"
---

Follow-up of habu-v2-structured-diagnostic-18d24536 (landed 6b19cda8): the diagnostic ADT's  field is a string and  is string[] because rev-id/evidence-id had no owner registries when the lane designed them - but maki/rev.f (package REV, rev-id COMMIT intern + ID>WIRE/WIRE>ID) landed mid-lane, so the revision promotion is NOW mechanical: serialize revision as CAD-KIND:rev-id via REV across the package boundary (wrong-width->malformed, unknown->bounds fold), update the four lossless-lowering fixtures + round-trip. The invalidated-evidence promotion waits on an evidence-id owner registry - that leg belongs to the evidence dots (habu-v2-evidence-applicability-73ac58b9 / habu-v2-evidence-promotion-f8312ebe should mint EVIDENCE per the plan-23.9 codec mechanism when they land; note added there). Also consider promoting parent-diagnostic from code n once diagnostics get durable identity (later dot). Files: maki/db/diagnostic.f, diagnostic-test.f. Ownership: V2 diagnostics.
