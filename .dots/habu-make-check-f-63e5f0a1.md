---
title: Make check.f check habu1.f and habu2.f from source
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.554445+02:00"
---

Problem: bin/hb --load tools/check.f -- src/habu/habu1.f fails with rc 70 E-UNDEFINED-TOP-LEVEL XDS at 34:4, and src/habu/habu2.f with rc 70 E-UNDEFINED LVLITPUSH in c-lit, on master before and after the comment prune: the engine build's load order supplies what these files need and check.f does not. Acceptance: decided at the responsible layer (check.f loads them in the engine build's order, or they carry a provided marker), both runs report rc 0 or E-ENGINE-PROVIDED. Files: tools/check.f, src/habu/habu1.f, src/habu/habu2.f. Verify: the two check.f runs. Depends: none. Ownership: tools/check.f. Claim: unassigned.
