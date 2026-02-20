---
title: Implement adaptive tenuring policy
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.462479+01:00"
blocks:
  - habu-implement-adaptive-nursery-08dfe594
---

File: src/runtime/gc.zig:1; cause: fixed promotion threshold causes premature/late promotion; fix: age+survival-based tenuring with feedback; why: lower copy cost and major pressure.
