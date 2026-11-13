---
title: Find a kernel symbol without a scan
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.060270+02:00"
---

Problem: src/habu/link-kernel.f KOBJ@ (:86) and SYM (:201) scan the kernel's symbols linearly, two accessor calls per symbol visited: 2.7M and 2.4M DYNAMIC-STORAGE:ADDRESS calls per warm engine build. Acceptance: a symbol lookup costs no scan of the table; calls measured before and after; warm equals cold. Depends: none. Claim: unassigned.
