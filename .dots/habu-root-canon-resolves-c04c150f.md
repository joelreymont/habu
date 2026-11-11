---
title: ROOT-CANON resolves without the supplied canon
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:58:41.734521+02:00"
---

Problem: src/core/include.f ROOT-CANON (ROOT-N) calls realpath directly while CWD-READ goes through SOURCE-INPUT:CANON; production installs only CANON-OS, test/include-refusal-e2e.f:141 installs another, so a root lookup and a working-directory lookup can disagree under a supplied canon (root-facts feature review, finding 7). Acceptance: one canon for both, by removing the supplied-canon cell if only the test swaps it (a hook where a plain call serves) or by routing ROOT-N through it. Files: src/core/include.f, test/include-refusal-e2e.f. Verify: test/include-refusal-e2e.f rc 0. Depends: habu-reuse-the-obj-a450cc5b landed. Ownership: those files. Claim: unassigned.
