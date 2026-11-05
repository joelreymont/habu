---
title: "Correct BF-DRV-SOURCE-KEEP's caller list"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T19:09:28.569837+02:00"
---

Problem: tools/build-fixpoint.f's comment above BF-DRV-SOURCE-KEEP says this file's chain driver, tools/aot-chain-bake.f and test/aot-wid-build.f all ask it, but its only caller is build-fixpoint.f itself (the word at BF-DRV-SOURCE-KEEP's one use, plus EXPORT BF-DRV-SOURCE-KEEP); neither other file names it (grep, 2026-11-05 tree). Acceptance: the comment names only its real caller; the EXPORT goes if nothing outside the file needs it. Files: tools/build-fixpoint.f. Verify: grep BF-DRV-SOURCE-KEEP over tools/ test/ src/; the build-fixpoint-source suite. Depends: none. Ownership: tools/build-fixpoint.f. Claim: unassigned.
