---
title: Find why the build profile charges read
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.499886+01:00"
---

Problem: tools/build-profile.f charges 1,349 samples of a warm engine build to read, while strace shows 8.9 ms in read calls; one of the two is wrong. Acceptance: the cause is named and fixed at its root; the profile and strace agree. Depends: none. Claim: unassigned.
