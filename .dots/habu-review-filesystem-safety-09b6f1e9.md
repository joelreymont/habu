---
title: Review filesystem safety changes
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T03:27:38.421318+02:00\""
---

Independent destruction review of current uncommitted fs-stream/fs-atomic/native/recovery work in .jj-ws/sol-safe-change. Inspect only: correctness, necessity, package/style compliance, checked type-system use, symlink/race safety, portability, error propagation, and focused test gaps. Do not edit the current tree. Report file:line findings and identify any complexity not justified by an invariant.
