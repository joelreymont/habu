---
title: "Make the declaration frame's one-shot seams private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T19:21:05.232419+02:00"
---

Problem: src/core/generated-declaration-protection.f:102-108 retires seven public words of sealed packages at startup with undefine (CHECKER-DECL-FRAME:PART-SNAPSHOT, -PREPARE, -ROLLBACK, -RELEASE; DECL-EVENT:PART-SNAPSHOT, -PREPARE, -COMMIT) so that only the compiled runner can reach them; :109-134 retire more words the same way. Restricting a word is a private section's job; a public word removed after its package is sealed is what the switch design's rule refusing every removal forbids after startup, and these run only because the seal floor is still 0. Acceptance: each word is private to its package and the undefine lines go, or, where another package's code must name it, the lane reports which and why. Depends: none. Claim: unassigned.
