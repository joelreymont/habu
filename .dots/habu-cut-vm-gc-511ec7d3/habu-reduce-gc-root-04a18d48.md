---
title: Reduce GC root costs
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.866764+01:00"
---

src/interp/vm.zig collect path. Cause: root-set assembly overhead dominates CAS loops. Fix: compact root staging and avoid duplicate root walks.
