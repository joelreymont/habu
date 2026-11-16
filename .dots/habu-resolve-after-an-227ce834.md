---
title: Resolve .. after an unsearchable directory as the kernel does
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.400536+01:00"
---

Problem: TRY-CANON's glibc realpath (src/core/include.f) collapses `locked/..` as text and checks only that `locked` is a directory, while the kernel's lookup of `..` needs search permission: with `locked` at mode 0, `require locked/../here2.f` loads and runs while `cat locked/../here2.f` fails EACCES. Acceptance: that require is rejected as a directory hb cannot search, by the load and by tools/check.f, as the OS would. Depends: none. Claim: unassigned.
