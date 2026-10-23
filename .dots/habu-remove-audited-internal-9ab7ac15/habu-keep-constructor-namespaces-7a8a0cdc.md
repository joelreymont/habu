---
title: Keep constructor namespaces readable
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.682894+03:00"
closed-at: "2026-10-22T12:36:35.093705+03:00"
close-reason: "Triage 2026-10-22, DONE: `type-family-sha` has no hits. src/core/type-family.f:1309-1324 uses the escaped spelling, which stays injective at every length up to CHECKER-PACKAGE-CAP; there is no hash."
---

type-family.f hashes escaped namespace above arbitrary32-character readability threshold, turning a33-character family into51 characters. Use escaped spelling to actual supported name boundary, retain collision/length refusal, remove type-family-sha and load/capture/reset bridge. Migrate in-tree hashed spellings and docs. Existing constructor publication/capture E2Es and meaningful long-name boundary behavior; source-matched native gate/convergence.
