---
title: "Lock the x86 profiler's control words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.883741+01:00"
---

Problem: the x86 profiler has no twin of prof.f's control lock (C-PROF-CTL-TAKE/FREE). Acceptance: x86 control words take the same lock as arm64's. Depends: none. Claim: unassigned.
