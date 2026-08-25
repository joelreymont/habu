---
title: src/core delete candidates
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.883724+02:00"
---

Problem: zero/one-consumer mechanisms measured by rg: src/core/sha-check.f (0 consumers, not in boot-pin); sha256.f:9-34 ROTR/SHR/CH/MAJ/BSIG*/SSIG* (SHA-BLOCK inlines the math); USIG-NEWEST-LINEAR, NORET-NEWEST-LINEAR, TFAM-FIND-IN-LINEAR, SUMV-CTOR-FIRST-LINEAR (one test file); RIGID-MAX writable only by a test; VALUE-RECORD + VREC arena (~450 lines, one production consumer maki/schedule.f - STRUCTURE replaces it); TL-NICHE/TL-BOXED/TL-CUSTOM/TK-EVIDENCE/PK-EVIDENCE kinds with no declarer; top-row.f tier-2 enabled only by two tests. Acceptance: each deleted (or moved into its only test) with the consumer count re-measured in the commit; no behaviour change in scheduled gates. Files: src/core/. Verify: full test/run.f. Depends: none. Ownership: src/core. Claim: unassigned.
