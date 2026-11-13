---
title: Point the x86-64 link-records test at a file that exists
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:08:07.694309+01:00"
---

Problem: test/x86-64-link-records.f:65 requires tools/native-emit-x64.f, which does not exist, and src/habu/kernel-x64.f:5771 cites it. Acceptance: the test requires what it exercises and the comment names a file that exists, when x86-64 is next built. Depends: none. Claim: unassigned.
