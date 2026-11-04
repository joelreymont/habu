---
title: "Make the declaration frame's one-shot seams private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T19:21:05.232419+02:00"
---

Problem: src/core/generated-declaration-protection.f:222-232 retires seven public words of sealed packages at startup with undefine (CHECKER-DECL-FRAME:PART-SNAPSHOT, -PREPARE, -ROLLBACK, -RELEASE; DECL-EVENT:PART-SNAPSHOT, -PREPARE, -COMMIT) so that only the compiled runner can reach them. Restricting a word is a private section's job (DECISIONS 2026-10-08, privacy); a public word removed after its package is sealed is what the switch design's refuse-every-removal rule (DECISIONS 2026-11-03) forbids after startup, and these run only because the seal floor is still 0. Acceptance: each word is private to its package and the undefine lines go, or, where another package's code must name it, the lane reports which and why. Files: src/core/generated-declaration-protection.f, the packages that define the seven words. Verify: the declaration and checker-surface rows that load the frame (test/checker-surface.f, test/package-seal.f) and a rebuilt bin/hb. Depends: none. Ownership: those files. Claim: unassigned.
