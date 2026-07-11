---
title: Document wide ADT memory lowering
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:34:17.018992+02:00"
---

Problem: commit 69fb5059 implements S2 wide non-linear ADT !/@ but docs/type-families.md still says W>1 waits and STATUS/LESSONS do not record the checker fact, pass-2 lowering, bootstrap proof, or limitations. Fix: update docs/type-families.md memory tier and slice status; add one concise LESSONS.md entry covering W0/address-capacity/seal-guard findings; update STATUS.md only if its verified claims can cite completed commands. Acceptance: docs distinguish W1 scalar, W>1 width-fact lowering, linear/open rejection, caller-owned allocation until LAYOUT-BUFFER, and per-cell protected-store guard; no code/API invention. Files: docs/type-families.md, LESSONS.md, optionally STATUS.md. Verify: rg stale W>1-waits text; bin/hb --load tools/filemap-lint.f; docs statements match commit 69fb5059 and bootstrap candidate results. Depends: none; implementation commit 69fb5059 is already on the integration line.
