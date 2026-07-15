---
title: Alias CAD-EFFECT SNAPSHOT/RESTORE to NOM words
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T18:59:53.579192+02:00"
---

Follow-up found by the effalias lane 2026-07-15 closing habu-replace-cad-effect-5f9d4166: src/cad/effect.f still keeps SNAPSHOT and RESTORE as bare pass-through bodies to NOM:SNAPSHOT/NOM:RESTORE (store-wide wire serialization, ptr u8 n -- n) - outside that dot's named five-word scope, deliberately not expanded. Same treatment applies: verify each is a pure delegation with no extra behavior, no consumer depends on distinct xts, then replace with EXPORT NOM:SNAPSHOT / EXPORT NOM:RESTORE in the public section (mechanism proven: alias carries the exact typed scheme; delete the body first or the reopen hits duplicate-tail rejection). Acceptance: bodies gone, effect suites x3 + nominal suites x3 + maki green, trust manifest unchanged, typed-local-diff clean. Files: src/cad/effect.f. Verify: those suites + host/filemap lints. Ownership: cad effect surface.
