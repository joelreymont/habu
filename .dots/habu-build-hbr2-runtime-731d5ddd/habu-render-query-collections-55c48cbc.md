---
title: Render query collections through EACH
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.585813+03:00"
---

Problem: HBR2 §7.2 stores an EACH collection descriptor and row component without iterating; §6 renders a query job's ordered collection through a bounded window; row components receive owned immutable props with the item's identity, field revisions and view flags, never a row index (§7.6 trace). Acceptance: in package UI-COLLECTION, a ui-collection over a habu-query-store-ranges-3c9ee413 result; EACH enumeration runs as a job that instantiates rows only for the visible window; row props carry the EntityId and revisions; a row's actions copy its EntityId, so a reorder or filter never retargets them; a new query revision reconciles rows by EntityId through habu-reconcile-keyed-children-beee6378. Files: lib/ui/collection.f (new, package UI-COLLECTION; mints E-UI-COLLECTION-FIRST/LAST -9740..-9749 in its owning file), lib/errors.f (one comment line), test/browser/collection-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/browser/collection-test.f: a 100,000-entity store shows a 50-row window and no callback reads beyond it; a SELECT action captured before a reorder still names its entity; bin/hb --load test/run.f. Depends: habu-reconcile-keyed-children-beee6378, habu-query-store-ranges-3c9ee413. Ownership: lib/ui/collection.f. Lane: tim. Claim: unassigned.
