---
title: Check image-x64.f and snap-x64.f alone
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:43:43.632540+01:00"
---

Problem: hb --load tools/check.f -- --verify-only src/habu/image-x64.f (and snap-x64.f) is rejected E-UNDEFINED MBUF at image-x64.f:48, on master as well: the file uses MBUF without requiring what defines it. Acceptance: both files pass check.f --verify-only. Depends: none. Claim: unassigned.
