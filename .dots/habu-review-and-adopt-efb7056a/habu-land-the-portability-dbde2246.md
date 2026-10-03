---
title: Land the portability design in docs
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.760489+03:00"
---

Problem: PA-r2, P1 and HBR2 live only outside the repository (copies under /private/tmp/claude-501/imports, reviewed in /private/tmp/claude-501/reports/pa-r2-summary.md and the Fable adoption review of 2026-10-03); docs/wasm-backend.md (baseline aed8416b) contradicts PA-r2 §18 and §21 and names W0-W7, which the campaign no longer uses. Acceptance: docs/portability.md holds PA-r2 with the review's section fixes (§0.1, §6, §7.4, §10.1, §15.3, §17, §20.1, §24.3-24.4, §26, §27 edges) and stale-claim corrections against master; docs/wasm-backend.md keeps §4-9 and the §16 rows and takes PA-r2 §17-21 plus the P6 first-slice route (NSHADOW, padded LEB, full-FREEZE WSTRUCT, NaN canonicalisation); docs/package-build.md holds P1 with its baseline header (37b2c1b7); docs/roadmap.md lists both under C6; campaign habu-campaign-webassembly-backend-6e7fcb57 acceptance re-pointed to these P-dots and its dead Depends removed. Files: docs/portability.md, docs/wasm-backend.md, docs/package-build.md, docs/roadmap.md, .dots/. Verify: every file:line cited in the docs resolves on master (rg); no reference to W0-W7 remains outside history. Depends: none. Ownership: docs/portability.md, docs/wasm-backend.md, docs/package-build.md. Lane: tim. Claim: unassigned.
