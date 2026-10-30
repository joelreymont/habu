---
title: Cut the section banners the landed prune batches kept
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.196408+02:00"
---

Problem: the landed src/compiler, src/lib and test comment batches kept section banners (839, 604 and 884 banner lines by the src/habu reviewer's count); src/habu/t0.f (24), t0-emit.f (12), t0-arm64.f (9), t0-x64.f (9) and t0-label.f (4) keep 58 more; src/habu/created-body-abi.f landed after the src/habu prune with a 30-line comment header. Acceptance: no banner lines (dash or equals rules, all-caps headings) in those files; headers at most six lines; code byte-identical (strip.py). Files: src/compiler, src/lib, test, src/habu/t0*.f, src/habu/created-body-abi.f. Verify: rg for the banner patterns prints nothing; strip.py 'code identical' per file; check.f per file. Depends: the tools, test, lib and core directory commits landing. Ownership: comments only. Claim: unassigned.
