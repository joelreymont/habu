---
title: Recover size-guard implementation half
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:35:34.579426+02:00"
---

Workspace forensic sweep 2026-07-18: master landed the size ATTRIBUTION half of the engine-size campaign (bin/hb size-report, byte reconciliation, committed manifest gate, merged 3ecdd106) but the size-GUARD half exists only in held workspaces: native bounds guards shared across emitters, wrapped-call clobber lint, package-aware shadow lint, engine size contracts, and the measured macOS ratchet. Held tips: size-guards 3c0b3f2a, size-guard-claims 9d93a1c0, size-guard-integration 96751d1b, size-guard-rebase 942a4117, habu-ratchet-measured-engine-b4032d74 ed3f83e3, habu-repair-bootstrap-shared-43b927d9 67190e20, habu-repair-shared-guard-5518ad25 3158b5c2. Task: identify the fullest coherent tip (likely size-guard-integration or size-guard-rebase - verify by diff), rebase onto current master, reconcile with the landed attribution surface (the guard rows should validate against the same ENGINE-SIZE table the attribution slice built), review, land or retire per piece with written reasons. The seven workspaces must not be deleted before this recovery. Part of the docs/size-campaign.md program toward the sub-128K engine.
