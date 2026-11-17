---
title: Keep a check of tools/install.f from installing
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:08:58.704451+01:00"
---

Problem: tools/check.f on tools/install.f installed the checked tree over ~/.local/lib/habu (carl's census, 2026-11-17 08:00 and 08:03): checking a tool runs its top level. Acceptance: checking any tools/*.f that acts on the file system (install.f first) changes nothing outside its scratch, and says so in a test. Depends: none. Claim: unassigned.
