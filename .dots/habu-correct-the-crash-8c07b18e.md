---
title: "Correct the crash test's region comment"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T15:52:54.108821+01:00"
---

Problem: test/x86-64-kernel-crash.f:297 says the code region is read-write; it is RX with a separate RW alias. Acceptance: the comment is true. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-skel.
