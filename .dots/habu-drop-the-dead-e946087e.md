---
title: Drop the dead checker-package label strings from the engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:16:25.187739+03:00"
---

Problem: src/habu/habu2.f still bakes checker-package, checker-public, checker-private, checker-end-package and checker-defer as label strings that nothing reads (each occurs only in its variable, the bake and the label allocation) since the registrar moved into the declaration-owner record; test/engine-error-package.f no longer patches them (restated 2026-09-12). Acceptance: the five labels, their variables and bakes removed; engine builds; engine-identity and engine-error-package suites green; two-generation chain byte-identical at (4,5). Files: src/habu/habu2.f. Verify: cold build, the two suites, tools/two-generation-build.f. Depends: none. Ownership: hazel. Claim: unassigned.
