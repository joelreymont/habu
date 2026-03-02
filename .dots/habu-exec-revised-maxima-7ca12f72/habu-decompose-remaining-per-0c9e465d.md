---
title: Decompose remaining per-file correctness failures after Checkpoint A
status: open
priority: 2
issue-type: task
created-at: "2026-03-07T19:32:55.812060+01:00"
blocks:
  - habu-fix-maxima-partition-147ac745
---

Checkpoint A follow-on from PLAN.md: split residual file-level failures into concrete per-rtest dots (rtest3/5/7/9a/10/12/13/15/16 and newly unblocked share tests). Root cause: correctness sweep cannot stay as a catch-all. Fix: create per-file dots once infrastructure blockers are removed. Why: turns Stage-5 correctness work into executable tasks.
