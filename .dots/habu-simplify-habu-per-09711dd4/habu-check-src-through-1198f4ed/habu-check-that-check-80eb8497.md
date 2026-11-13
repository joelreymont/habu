---
title: "Check that check.f's dependency closure keys records by the open package"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T08:47:42.374108+02:00"
---

Problem: tools/check-verify-core.f (check.f's dependency closure) harvests discovery records with the same file-local package keying that lane disc replaces in tools/source-discovery.f (habu-key-discovery-records-b367e77e), and never runs the context replay, so it likely misses the same loads (fixtures 5a, 5a0, kid, kidpart of the pkg2b review; their files are lost). Not measured yet (disc worker, by reading). Acceptance: run check.f's closure on the four fixtures against the engine's own load (dep.f prints LOADED); if it misses one, key its records by the open package through the same replay discovery uses, with a case per shape in check.f's test failing before. Files: tools/check-verify-core.f (~150-345), tools/source-discovery.f. Depends: lane disc landed. Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
